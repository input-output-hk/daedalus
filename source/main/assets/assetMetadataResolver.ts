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

export type AssetMetadataResolverOptions = {
  database?: AssetMetadataDatabase;
  transport?: RegistryTransport;
  endpoint?: string | null;
  onResolved?: (rows: Array<AssetMetadataRow>) => void;
  now?: () => number;
  retryBackoffMs?: number;
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

  private _claimed = new Set<string>();

  private _pending: Promise<void> = Promise.resolve();

  constructor(options: AssetMetadataResolverOptions = {}) {
    this._db = options.database ?? openAssetMetadataDatabase();
    this._transport = options.transport;
    this._endpoint = options.endpoint;
    this._onResolved = options.onResolved;
    this._now = options.now ?? Date.now;
    this._retryBackoffMs = options.retryBackoffMs;
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
  request(subjects: Array<string>): Array<AssetMetadataRow> {
    const rows = this.readCached(subjects);
    const claimed = this._claim(this._due(subjects, rows));
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

    if (toWrite.length > 0) this._db.writeMetadata(toWrite, now);
    if (resolutions.length > 0) this._db.writeResolutions(resolutions, now);

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
