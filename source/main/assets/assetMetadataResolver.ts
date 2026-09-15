import { logger } from '../utils/logging';
import {
  openAssetMetadataDatabase,
  AssetMetadataDatabase,
} from './assetMetadataDb';
import type {
  AssetMetadataRow,
  AssetMetadataWrite,
  AssetResolutionWrite,
} from './assetMetadataDb';
import { queryAssetRegistry } from './assetRegistryClient';
import type {
  RegistryEntry,
  RegistryProperty,
  RegistryTransport,
} from './assetRegistryClient';
import { verifyRegistryProperty } from './assetVerification';
import { queryKoiosPointers } from './koiosClient';
import type {
  KoiosPointer,
  KoiosRequestBudget,
  KoiosTransaction,
} from './koiosClient';
import type { HttpTransport } from './httpTransport';
import { confirmChainPointer } from './chainPointerVerification';
import { ImmutableBlockReader } from './immutableBlockReader';

const POLICY_ID_HEX_LENGTH = 56;
const MAX_DECIMAL_PRECISION = 20;

/**
 * A row older than this is re-read on the next demand for it, never on a timer.
 *
 * Set from measurement: of 7,976 registry mapping files, 7,464 sit at sequence
 * number 0 on every property, and in a year 375 commits touched the mappings
 * directory while only 38 files were modified rather than added. That is an
 * average over the whole registry and not over the assets a user holds, which
 * skew toward active projects; nobody has measured how far above the average
 * they sit.
 */
export const ASSET_METADATA_REFRESH_MS = 7 * 24 * 60 * 60 * 1000;

/**
 * How long a subject waits when its pointer names a block the immutable
 * database does not hold yet.
 *
 * The window is the last k blocks, which on mainnet is about twelve hours. An
 * hour is comfortably inside it and costs two requests per retry for the whole
 * batch, so a freshly minted asset picks up its name within an hour of the
 * block settling rather than at the next cold start.
 */
export const ASSET_CHAIN_PENDING_RETRY_MS = 60 * 60 * 1000;

/**
 * How long a subject waits when its pointer was refused by the local check.
 *
 * A rejection is a fact about that pointer rather than a transient failure, but
 * the pointer can change: an index can correct itself, and `minting_tx_hash` is
 * documented as both the first and the latest mint. A day is long enough that a
 * lying index is not re-asked at any cost worth measuring.
 */
export const ASSET_CHAIN_REJECTED_RETRY_MS = 24 * 60 * 60 * 1000;

export type AssetMetadataResolverOptions = {
  database?: AssetMetadataDatabase;
  transport?: RegistryTransport;
  endpoint?: string | null;
  onResolved?: (rows: Array<AssetMetadataRow>) => void;
  now?: () => number;
  retryBackoffMs?: number;
  /** The immutable database the chain channel confirms pointers against. */
  immutableDirectory?: string | null;
  /** The user's selected pointer source. Null disables the chain channel. */
  pointerSourceUrl?: string | null;
  pointerTransport?: HttpTransport;
  pointerBudget?: KoiosRequestBudget;
};

const asString = (value: unknown): string | null =>
  typeof value === 'string' && value.length > 0 ? value : null;

const propertyValue = (
  entry: RegistryEntry,
  name: string
): RegistryProperty | null => entry.properties[name] ?? null;

const stringProperty = (entry: RegistryEntry, name: string): string | null =>
  asString(propertyValue(entry, name)?.value);

/**
 * Out of range stores null rather than losing the row. `asset_metadata` refuses
 * a value above MAX_DECIMAL_PRECISION, and writing one would make the engine
 * reject the whole row, taking the ticker and the name with it. cardano-wallet
 * accepts 0 to 255, so a registry value above 20 is possible.
 */
const decimalsValue = (entry: RegistryEntry): number | null => {
  const value = propertyValue(entry, 'decimals')?.value;
  if (!Number.isInteger(value)) return null;
  const decimals = value as number;
  if (decimals < 0 || decimals > MAX_DECIMAL_PRECISION) return null;
  return decimals;
};

const metadataJson = (entry: RegistryEntry): string | null => {
  const url = stringProperty(entry, 'url');
  const description = stringProperty(entry, 'description');
  if (url === null && description === null) return null;
  return JSON.stringify({
    ...(url === null ? {} : { url }),
    ...(description === null ? {} : { description }),
  });
};

const maxSequenceNumber = (entry: RegistryEntry): number | null => {
  const numbers = Object.keys(entry.properties).map(
    (name) => entry.properties[name].sequenceNumber
  );
  if (numbers.length === 0) return null;
  return numbers.reduce((highest, value) => Math.max(highest, value));
};

/**
 * The schema carries one `verified` column and the PRD asks for per-property
 * verification, so the column is given one meaning: it is the verdict for the
 * `decimals` property of this row.
 *
 * Every behaviour the PRD describes for the column is about decimal places. The
 * resolution order applies the cached value if and only if it is verified, the
 * corpus is binned by the decimals property's outcome, and the advisory says
 * that the published decimal places could not be verified. Names and tickers
 * are display-only and are written whether or not they verified.
 */
const verifiedDecimals = (entry: RegistryEntry): boolean => {
  const property = propertyValue(entry, 'decimals');
  if (!property) return false;
  return verifyRegistryProperty(
    entry.subject,
    entry.policy,
    'decimals',
    property
  ).verified;
};

export const registryEntryToRow = (
  entry: RegistryEntry
): AssetMetadataWrite => ({
  subject: entry.subject,
  policyId: entry.subject.slice(0, POLICY_ID_HEX_LENGTH),
  assetName: entry.subject.slice(POLICY_ID_HEX_LENGTH),
  ticker: stringProperty(entry, 'ticker'),
  name: stringProperty(entry, 'name'),
  decimals: decimalsValue(entry),
  // Computed here from the bytes. No field of a registry response sets it.
  verified: verifiedDecimals(entry),
  metadata: metadataJson(entry),
  source: 'registry',
  sequenceNumber: maxSequenceNumber(entry),
  slot: null,
});

/**
 * A confirmed pointer as a row.
 *
 * Four of the columns are fixed and each is a rule rather than a default.
 * `source` is `chain`. `slot` is the mint block's, which is what a chain row has
 * instead of a sequence number. `sequence_number` is NULL, and the schema's
 * CHECK refuses a chain row that carries one. `decimals` is NULL, which is the
 * mechanical form of the rule that no amount is ever formatted by a number that
 * did not come from the registry. `verified` is false, because `verified` is the
 * verdict of the registry attestation chain and a chain row never runs it.
 *
 * The name is the CIP-25 `name`, or the CIP-68 `name` when there is no CIP-25
 * record. Where an index answers with both, the CIP-68 datum is the live record
 * and is the one stored.
 *
 * A CIP-68 value sits on weaker footing than a CIP-25 one and the difference is
 * not visible in the row. A CIP-25 payload is in the mint transaction, so the
 * block read confirms it. A CIP-68 datum lives at a spendable UTxO, which
 * changes whenever that output is spent, so the mint transaction says nothing
 * about its current value and confirming it would mean querying the live UTxO
 * set. That is acceptable only because of what the value is used for, which is a
 * name and nothing else.
 */
export const chainPointerToRow = (
  pointer: KoiosPointer,
  slot: number,
  cip25: Record<string, unknown> | null
): AssetMetadataWrite => {
  const payload = pointer.cip68Metadata ?? cip25;
  const metadata = payload ? JSON.stringify(payload) : null;
  return {
    subject: pointer.subject,
    policyId: pointer.policyId,
    assetName: pointer.assetName,
    ticker: null,
    name: chainName(payload),
    decimals: null,
    verified: false,
    metadata,
    source: 'chain',
    sequenceNumber: null,
    slot,
  };
};

/**
 * The name inside a CIP-25 or CIP-68 payload.
 *
 * CIP-25 version 1 writes a bare string; a payload built from a metadatum whose
 * value was a one-element array carries `['Name']`, which is how the ledger
 * splits a string over 64 bytes and also how some minters write a single value.
 * Both spellings are read, and anything else is no name rather than a rendered
 * object.
 */
const chainName = (payload: Record<string, unknown> | null): string | null => {
  if (!payload) return null;
  const value = payload.name;
  if (typeof value === 'string' && value.length > 0) return value;
  if (Array.isArray(value)) {
    const joined = value
      .filter((part): part is string => typeof part === 'string')
      .join('');
    return joined.length > 0 ? joined : null;
  }
  return null;
};

const sameContent = (
  row: AssetMetadataWrite,
  stored: AssetMetadataRow
): boolean =>
  row.ticker === stored.ticker &&
  row.name === stored.name &&
  row.decimals === stored.decimals &&
  row.verified === stored.verified &&
  row.metadata === stored.metadata &&
  row.source === stored.source &&
  row.sequenceNumber === stored.sequenceNumber &&
  row.slot === stored.slot;

const storedAsWrite = (stored: AssetMetadataRow): AssetMetadataWrite => ({
  subject: stored.subject,
  policyId: stored.policyId,
  assetName: stored.assetName,
  ticker: stored.ticker,
  name: stored.name,
  decimals: stored.decimals,
  verified: stored.verified,
  metadata: stored.metadata,
  source: stored.source,
  sequenceNumber: stored.sequenceNumber,
  slot: stored.slot,
});

export class AssetMetadataResolver {
  private _db: AssetMetadataDatabase;

  private _transport?: RegistryTransport;

  private _endpoint?: string | null;

  private _onResolved?: (rows: Array<AssetMetadataRow>) => void;

  private _now: () => number;

  private _retryBackoffMs?: number;

  private _immutableDirectory?: string | null;

  private _pointerSourceUrl?: string | null;

  private _pointerTransport?: HttpTransport;

  private _pointerBudget?: KoiosRequestBudget;

  private _claimed = new Set<string>();

  private _pending: Promise<void> = Promise.resolve();

  constructor(options: AssetMetadataResolverOptions = {}) {
    this._db = options.database ?? openAssetMetadataDatabase();
    this._transport = options.transport;
    this._endpoint = options.endpoint;
    this._onResolved = options.onResolved;
    this._now = options.now ?? Date.now;
    this._retryBackoffMs = options.retryBackoffMs;
    this._immutableDirectory = options.immutableDirectory;
    this._pointerSourceUrl = options.pointerSourceUrl;
    this._pointerTransport = options.pointerTransport;
    this._pointerBudget = options.pointerBudget;
  }

  /**
   * The pointer source the renderer last named.
   *
   * The setting is the renderer's, per profile, and the client is here, so it
   * arrives with each read rather than on a channel of its own. Setting it is a
   * plain assignment: nothing is scheduled by a change, and the next resolution
   * uses whatever is current.
   */
  setPointerSourceUrl(sourceUrl: string | null | undefined): void {
    this._pointerSourceUrl = sourceUrl;
  }

  close(): void {
    this._db.close();
  }

  /** Answers from disk. Never waits on the network. */
  readCached(subjects: Array<string>): Array<AssetMetadataRow> {
    return this._db.readMetadata(subjects);
  }

  /**
   * Reads from disk and starts whatever is due. The read is what a caller gets
   * back; the fetch fills the misses for the next one.
   *
   * Subjects are claimed synchronously, before the work is queued, so a second
   * call arriving in the same tick does not schedule the same subject twice.
   */
  request(
    subjects: Array<string>,
    options: { force?: boolean } = {}
  ): Array<AssetMetadataRow> {
    const rows = this.readCached(subjects);
    // A forced read skips the refresh window and the retry backoff, and skips
    // them by not consulting them rather than by clearing the columns they are
    // read from. Clearing `updated_at` before the fetch would leave a row that
    // looks never-updated if the fetch then failed, and every render afterwards
    // would re-schedule it.
    const due =
      options.force === true
        ? subjects.filter((subject) => !this._claimed.has(subject))
        : this._due(subjects, rows);
    const claimed = this._claim(due);
    if (claimed.length > 0) {
      this._pending = this._pending.then(async () => {
        try {
          await this._run(claimed);
        } catch (error) {
          logger.debug('Asset metadata: background resolve failed', {
            reason: error instanceof Error ? error.message : 'unknown',
          });
        } finally {
          this._release(claimed);
        }
      });
    }
    return rows;
  }

  /** Resolves after the work started by `request` has settled. */
  pending(): Promise<void> {
    return this._pending;
  }

  async resolve(subjects: Array<string>): Promise<Array<AssetMetadataRow>> {
    const claimed = this._claim(subjects);
    if (claimed.length === 0) return [];
    try {
      return await this._run(claimed);
    } finally {
      this._release(claimed);
    }
  }

  private _claim(subjects: Array<string>): Array<string> {
    const claimed = subjects.filter((subject) => !this._claimed.has(subject));
    claimed.forEach((subject) => this._claimed.add(subject));
    return claimed;
  }

  private _release(subjects: Array<string>): void {
    subjects.forEach((subject) => this._claimed.delete(subject));
  }

  private async _run(wanted: Array<string>): Promise<Array<AssetMetadataRow>> {
    const now = this._now();
    const stored = new Map(
      this._db.readMetadata(wanted).map((row) => [row.subject, row])
    );
    const failureCounts: Record<string, number> = {};
    this._db.readResolutions(wanted).forEach((row) => {
      failureCounts[row.subject] = row.failureCount;
    });

    let entries: Array<RegistryEntry> = [];
    let resolutions: Array<AssetResolutionWrite> = [];
    try {
      // No database call inside the awaited section, and no transaction held
      // across it.
      const result = await queryAssetRegistry(wanted, {
        transport: this._transport,
        endpoint: this._endpoint,
        retryBackoffMs: this._retryBackoffMs,
        failureCounts,
        now,
      });
      entries = result.entries;
      resolutions = result.resolutions;
    } catch (error) {
      // Offline is a state, not a failure. Nothing is surfaced to the user.
      logger.debug('Asset metadata: query failed', {
        reason: error instanceof Error ? error.message : 'unknown',
        subjectCount: wanted.length,
      });
    }

    const toWrite: Array<AssetMetadataWrite> = [];
    const changed: Array<string> = [];
    entries.forEach((entry) => {
      const built = registryEntryToRow(entry);
      const previous = stored.get(entry.subject);
      if (!previous) {
        toWrite.push(built);
        changed.push(entry.subject);
        return;
      }
      if (this._supersedes(built, previous)) {
        toWrite.push(built);
        if (!sameContent(built, previous)) changed.push(entry.subject);
        return;
      }
      // Stamped on every successful read whether or not anything changed, which
      // is what stops a never-updated subject being re-read on every render.
      // Written, but not emitted: nothing downstream has anything to do with a
      // row that did not change.
      toWrite.push(storedAsWrite(previous));
    });

    // Only subjects the registry did not answer reach the chain channel. That
    // is what keeps fungible holdings, which the registry does answer for, off
    // it entirely.
    const answered = new Set(entries.map((entry) => entry.subject));
    stored.forEach((row, subject) => {
      if (row.source === 'registry') answered.add(subject);
    });
    const unanswered = wanted.filter((subject) => !answered.has(subject));
    const chain = await this._resolveFromChain(unanswered, failureCounts, now);
    chain.rows.forEach((row) => {
      toWrite.push(row);
      changed.push(row.subject);
    });

    if (toWrite.length > 0) this._db.writeMetadata(toWrite, now);
    // The chain outcome is written after the registry's for the same subject,
    // and both are an upsert on the subject, so the later one stands. That is
    // the right way round: the registry recorded a subject it does not know,
    // and the chain channel has just said something more specific about it.
    const allResolutions = resolutions.concat(chain.resolutions);
    if (allResolutions.length > 0) {
      this._db.writeResolutions(allResolutions, now);
    }

    const emitted = changed.length > 0 ? this._db.readMetadata(changed) : [];
    if (emitted.length > 0 && this._onResolved) {
      try {
        this._onResolved(emitted);
      } catch (error) {
        logger.debug('Asset metadata: consumer threw on resolved rows', {
          reason: error instanceof Error ? error.message : 'unknown',
        });
      }
    }
    return emitted;
  }

  /**
   * The chain channel: ask the index for pointers, confirm each against the
   * user's own chain, and turn the confirmations into rows.
   *
   * Nothing here throws. A subject that cannot be confirmed produces a
   * resolution row and no metadata row, which is the same shape the registry
   * channel already uses for a subject it could not answer.
   */
  private async _resolveFromChain(
    subjects: Array<string>,
    failureCounts: Record<string, number>,
    now: number
  ): Promise<{
    rows: Array<AssetMetadataWrite>;
    resolutions: Array<AssetResolutionWrite>;
  }> {
    const empty = { rows: [], resolutions: [] };
    if (subjects.length === 0) return empty;
    if (!this._pointerSourceUrl || !this._immutableDirectory) return empty;

    let result;
    try {
      result = await queryKoiosPointers(subjects, {
        baseUrl: this._pointerSourceUrl,
        transport: this._pointerTransport,
        budget: this._pointerBudget,
        failureCounts,
        retryBackoffMs: this._retryBackoffMs,
        now,
      });
    } catch (error) {
      logger.debug('Asset metadata: pointer query failed', {
        reason: error instanceof Error ? error.message : 'unknown',
        subjectCount: subjects.length,
      });
      return empty;
    }

    if (result.pointers.length === 0) {
      return { rows: [], resolutions: result.resolutions };
    }

    // One reader for the pass. It lists the immutable directory once to find
    // the tip, which on a synced mainnet is tens of thousands of entries.
    const reader = new ImmutableBlockReader(this._immutableDirectory);
    const byHash = new Map<string, KoiosTransaction>(
      result.transactions.map((transaction) => [
        transaction.txHash,
        transaction,
      ])
    );

    const rows: Array<AssetMetadataWrite> = [];
    const resolutions: Array<AssetResolutionWrite> = result.resolutions.slice();

    result.pointers.forEach((pointer) => {
      const transaction = byHash.get(pointer.mintingTxHash);
      if (!transaction) return;
      const confirmation = confirmChainPointer(
        {
          subject: pointer.subject,
          policyId: pointer.policyId,
          assetName: pointer.assetName,
          txHash: transaction.txHash,
          blockHash: transaction.blockHash,
          absoluteSlot: transaction.absoluteSlot,
          cbor: transaction.cbor,
        },
        { reader }
      );

      if (confirmation.status === 'pending') {
        resolutions.push({
          subject: pointer.subject,
          state: 'pending',
          failureCount: failureCounts[pointer.subject] ?? 0,
          retryAfter: now + ASSET_CHAIN_PENDING_RETRY_MS,
        });
        return;
      }
      if (confirmation.status === 'rejected') {
        logger.warn('Asset metadata: pointer refused by the local check', {
          reason: confirmation.reason,
        });
        resolutions.push({
          subject: pointer.subject,
          state: 'failed',
          failureCount: (failureCounts[pointer.subject] ?? 0) + 1,
          retryAfter: now + ASSET_CHAIN_REJECTED_RETRY_MS,
        });
        return;
      }
      if (confirmation.status === 'unavailable') {
        // Nothing was decided about the pointer, so nothing is recorded about
        // it either. The subject keeps whatever the registry pass wrote.
        return;
      }

      rows.push(
        chainPointerToRow(pointer, confirmation.slot, confirmation.cip25)
      );
      resolutions.push({
        subject: pointer.subject,
        state: 'resolved',
        failureCount: 0,
        retryAfter: 0,
      });
    });

    return { rows, resolutions };
  }

  private _supersedes(
    built: AssetMetadataWrite,
    previous: AssetMetadataRow
  ): boolean {
    // The registry wins where both channels could answer, and a chain row
    // carries no sequence number to compare against.
    if (previous.source !== 'registry') return true;
    // null means nothing is known about the stored version, not that it is at
    // zero, which is what it would coerce to.
    if (previous.sequenceNumber === null) return true;
    if (built.sequenceNumber === null) return false;
    return built.sequenceNumber > previous.sequenceNumber;
  }

  private _due(
    subjects: Array<string>,
    rows: Array<AssetMetadataRow>
  ): Array<string> {
    const now = this._now();
    const bySubject = new Map(rows.map((row) => [row.subject, row]));
    const blocked = new Set(
      this._db
        .readResolutions(subjects)
        .filter((row) => row.retryAfter > now)
        .map((row) => row.subject)
    );
    return subjects.filter((subject) => {
      if (blocked.has(subject)) return false;
      if (this._claimed.has(subject)) return false;
      const row = bySubject.get(subject);
      if (!row) return true;
      return now - row.updatedAt > ASSET_METADATA_REFRESH_MS;
    });
  }
}

export const openAssetMetadataResolver = (
  options: AssetMetadataResolverOptions = {}
): AssetMetadataResolver => new AssetMetadataResolver(options);
