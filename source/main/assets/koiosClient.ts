import { logger } from '../utils/logging';
import type { AssetResolutionWrite } from './assetMetadataDb';
import { httpTransport } from './httpTransport';
import type { HttpTransport, HttpTransportResult } from './httpTransport';

const POLICY_ID_HEX_LENGTH = 56;

/**
 * The fields the pointer needs, and no others.
 *
 * `logo` is never requested: measured 2026-09-11 for subject
 * `c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b942544544`, a trimmed
 * `asset_info` is 351 bytes against 78,821 untrimmed, and the base64 logo is
 * 98.2 percent of the difference.
 *
 * `minting_tx_metadata` is not requested either, and that is a stronger
 * statement than trimming. The CIP-25 payload is read out of the transaction
 * bytes the local confirmation has already checked, so asking the index for its
 * own copy would introduce an unconfirmed input for a value we already hold.
 * `cip68_metadata` is requested, because a CIP-68 datum lives at a spendable
 * UTxO rather than in the mint transaction and no local read of that
 * transaction can produce it.
 */
export const KOIOS_ASSET_INFO_FIELDS = [
  'policy_id',
  'asset_name',
  'fingerprint',
  'minting_tx_hash',
  'mint_cnt',
  'cip68_metadata',
];

export const KOIOS_TX_CBOR_FIELDS = [
  'tx_hash',
  'block_hash',
  'absolute_slot',
  'block_height',
  'cbor',
];

/** Matches the registry client. One wall-clock budget per request. */
export const KOIOS_TIMEOUT_MS = 10000;

export const KOIOS_RETRY_BACKOFF_MS = 1000;

/**
 * A batch of raw transactions is an order larger than a registry answer. A
 * single `tx_cbor` record measured 3,251 bytes, and a batch is bounded by
 * subject count rather than by bytes, so the cap is sized from the worst
 * plausible batch rather than from the measured one.
 */
export const KOIOS_MAX_RESPONSE_BYTES = 8 * 1024 * 1024;

/**
 * Both endpoints take arrays, so batching is what makes this two requests for a
 * wallet rather than two per asset. Bounded by count because a subject is a
 * fixed-length identifier and the request body is small either way.
 */
export const KOIOS_MAX_SUBJECTS_PER_BATCH = 100;

/**
 * The per-IP ceiling, carried here from the research note rather than left in
 * it.
 *
 * The public tier is rate limited at 100 requests per 10 seconds, enforced by
 * the operator's HAProxy on source address
 * (`research/01-koios-pointer-option.md:57-76`), with a daily cap of 5,000. A
 * desktop wallet resolving each asset once for the life of an installation has
 * no reason to approach either, so the ceiling here is a fifth of the published
 * one: it is a guard against a defect in this code, not a budget to spend.
 *
 * It counts requests in this process since it started. A restart clears it,
 * which is stated rather than worked around: persisting a counter would make a
 * bug survive a restart, and the demand model does not need it to.
 */
export const KOIOS_MAX_REQUESTS_PER_WINDOW = 20;
export const KOIOS_RATE_WINDOW_MS = 10000;

/**
 * How long a subject waits after the ceiling is reached or the instance answers
 * `429`. Long enough that a wallet which hit either is not straight back, short
 * enough that the subject resolves within a session.
 */
export const KOIOS_THROTTLED_RETRY_MS = 10 * 60 * 1000;

export const KOIOS_BACKOFF_BASE_MS = 5 * 60 * 1000;
export const KOIOS_BACKOFF_CEILING_MS = 24 * 60 * 60 * 1000;

export type KoiosPointer = {
  subject: string;
  policyId: string;
  assetName: string;
  fingerprint: string | null;
  mintingTxHash: string;
  mintCount: number | null;
  cip68Metadata: Record<string, unknown> | null;
};

export type KoiosTransaction = {
  txHash: string;
  blockHash: string;
  absoluteSlot: number;
  blockHeight: number | null;
  cbor: string;
};

export type KoiosQueryResult = {
  pointers: Array<KoiosPointer>;
  transactions: Array<KoiosTransaction>;
  resolutions: Array<AssetResolutionWrite>;
};

export type KoiosQueryOptions = {
  baseUrl?: string | null;
  transport?: HttpTransport;
  failureCounts?: Record<string, number>;
  retryBackoffMs?: number;
  now?: number;
  budget?: KoiosRequestBudget;
};

/**
 * A rolling count of requests issued from this process.
 *
 * Nothing here waits for room. Reaching the ceiling abandons the batch and
 * leaves the rows absent, which is the fail-closed behaviour the design asks
 * for: a token renders with its fingerprint and its decoded name whether or not
 * this channel ever answers, so there is nothing to block.
 */
export class KoiosRequestBudget {
  private _issued: Array<number> = [];

  private _limit: number;

  private _windowMs: number;

  constructor(
    limit: number = KOIOS_MAX_REQUESTS_PER_WINDOW,
    windowMs: number = KOIOS_RATE_WINDOW_MS
  ) {
    this._limit = limit;
    this._windowMs = windowMs;
  }

  tryConsume(now: number): boolean {
    const floor = now - this._windowMs;
    this._issued = this._issued.filter((issued) => issued > floor);
    if (this._issued.length >= this._limit) return false;
    this._issued.push(now);
    return true;
  }
}

const sharedBudget = new KoiosRequestBudget();

export const koiosBackoffMs = (failureCount: number): number => {
  const exponent = Math.max(0, failureCount - 1);
  const scaled = KOIOS_BACKOFF_BASE_MS * 2 ** exponent;
  return Math.min(scaled, KOIOS_BACKOFF_CEILING_MS);
};

export const koiosBatches = (subjects: Array<string>): Array<Array<string>> => {
  const batches: Array<Array<string>> = [];
  for (
    let index = 0;
    index < subjects.length;
    index += KOIOS_MAX_SUBJECTS_PER_BATCH
  ) {
    batches.push(subjects.slice(index, index + KOIOS_MAX_SUBJECTS_PER_BATCH));
  }
  return batches;
};

/**
 * The base is the user's selection. There is no fallback literal: a profile
 * with no selection and a network with no preset has no pointer source, and
 * inventing one here would send a selfnode user's subjects to a mainnet index.
 */
export const koiosEndpoint = (
  baseUrl: string | null | undefined,
  path: string,
  fields: Array<string>
): string | null => {
  if (!baseUrl || baseUrl === 'direct') return null;
  return `${baseUrl.replace(/\/+$/, '')}/${path}?select=${fields.join(',')}`;
};

const asString = (value: unknown): string | null =>
  typeof value === 'string' && value.length > 0 ? value : null;

const asNumber = (value: unknown): number | null =>
  typeof value === 'number' && Number.isFinite(value) ? value : null;

const asObject = (value: unknown): Record<string, unknown> | null =>
  value && typeof value === 'object' && !Array.isArray(value)
    ? (value as Record<string, unknown>)
    : null;

/**
 * `cip68_metadata` is keyed by asset name in Koios's answer. The value under
 * that key is what a CIP-68 reference datum decodes to.
 */
const cip68Of = (
  value: unknown,
  assetName: string
): Record<string, unknown> | null => {
  const map = asObject(value);
  if (!map) return null;
  const entry = asObject(map[assetName]);
  return entry ?? map;
};

const toPointer = (
  value: unknown,
  requested: Set<string>
): KoiosPointer | null => {
  const record = asObject(value);
  if (!record) return null;
  const policyId = asString(record.policy_id);
  // An asset with an empty name is a real asset, so the name is read as a
  // string that may be empty rather than through the non-empty helper.
  const assetName =
    typeof record.asset_name === 'string' ? record.asset_name : '';
  const mintingTxHash = asString(record.minting_tx_hash);
  if (!policyId || !mintingTxHash) return null;
  const subject = `${policyId}${assetName}`;
  if (!requested.has(subject)) {
    // An index that answers a question nobody asked must not create a row.
    logger.debug('Koios: response entry was not requested');
    return null;
  }
  return {
    subject,
    policyId,
    assetName,
    fingerprint: asString(record.fingerprint),
    mintingTxHash,
    mintCount: asNumber(record.mint_cnt),
    cip68Metadata: cip68Of(record.cip68_metadata, assetName),
  };
};

const toTransaction = (
  value: unknown,
  requested: Set<string>
): KoiosTransaction | null => {
  const record = asObject(value);
  if (!record) return null;
  const txHash = asString(record.tx_hash);
  const blockHash = asString(record.block_hash);
  const absoluteSlot = asNumber(record.absolute_slot);
  const cbor = asString(record.cbor);
  if (!txHash || !blockHash || absoluteSlot === null || !cbor) return null;
  if (!requested.has(txHash)) {
    logger.debug('Koios: transaction was not requested');
    return null;
  }
  return {
    txHash,
    blockHash,
    absoluteSlot,
    blockHeight: asNumber(record.block_height),
    cbor,
  };
};

const parseArray = (body: string): Array<unknown> | null => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(body);
  } catch {
    return null;
  }
  return Array.isArray(parsed) ? parsed : null;
};

const delay = (ms: number): Promise<void> =>
  new Promise((resolve) => {
    if (ms <= 0) {
      resolve();
      return;
    }
    setTimeout(resolve, ms);
  });

/**
 * A `429` is deliberately absent from this. The instance has said it is being
 * asked too often, and the answer to that is to stop, not to ask again after a
 * second. It is handled at the call site as a throttle rather than as a
 * failure.
 */
const isRetryable = (result: HttpTransportResult): boolean => {
  if (result.ok === false) return result.reason !== 'too-large';
  return result.status >= 500;
};

type Outcome = 'ok' | 'throttled' | 'failed';

type Answer = {
  outcome: Outcome;
  records: Array<unknown>;
};

const failed = (): Answer => ({ outcome: 'failed', records: [] });

async function send(
  url: string,
  body: string,
  transport: HttpTransport,
  budget: KoiosRequestBudget,
  retryBackoffMs: number,
  now: number
): Promise<Answer> {
  if (!budget.tryConsume(now)) {
    logger.debug('Koios: per-process request ceiling reached');
    return { outcome: 'throttled', records: [] };
  }

  let result = await transport.post(
    url,
    body,
    KOIOS_TIMEOUT_MS,
    KOIOS_MAX_RESPONSE_BYTES
  );
  if (isRetryable(result)) {
    await delay(retryBackoffMs);
    if (!budget.tryConsume(now)) {
      return { outcome: 'throttled', records: [] };
    }
    result = await transport.post(
      url,
      body,
      KOIOS_TIMEOUT_MS,
      KOIOS_MAX_RESPONSE_BYTES
    );
  }

  if (result.ok === false) {
    logger.debug('Koios: batch abandoned', { reason: result.reason });
    return failed();
  }

  if (result.status === 429) {
    logger.debug('Koios: instance refused, backing off');
    return { outcome: 'throttled', records: [] };
  }

  if (result.status < 200 || result.status >= 300) {
    logger.debug('Koios: batch abandoned', { status: result.status });
    return failed();
  }

  const records = parseArray(result.body);
  if (records === null) {
    logger.debug('Koios: response could not be read');
    return failed();
  }
  return { outcome: 'ok', records };
}

/**
 * Two calls per batch and never one.
 *
 * `asset_info` carries no chain point, and no other asset endpoint does either,
 * so a usable pointer always costs a second call. `tx_cbor` returns the point
 * and the transaction bytes together, which makes it a superset of `tx_info`
 * for this purpose and removes the reason to call both.
 *
 * Only subjects are sent. No wallet identifier, no address and no quantity, and
 * the channel is consulted only for subjects the registry did not answer, so
 * fungible holdings never reach it.
 */
export async function queryKoiosPointers(
  subjects: Array<string>,
  options: KoiosQueryOptions = {}
): Promise<KoiosQueryResult> {
  const distinct = Array.from(new Set(subjects));
  const empty: KoiosQueryResult = {
    pointers: [],
    transactions: [],
    resolutions: [],
  };
  if (distinct.length === 0) return empty;

  const assetInfoUrl = koiosEndpoint(
    options.baseUrl,
    'asset_info',
    KOIOS_ASSET_INFO_FIELDS
  );
  const txCborUrl = koiosEndpoint(
    options.baseUrl,
    'tx_cbor',
    KOIOS_TX_CBOR_FIELDS
  );
  if (!assetInfoUrl || !txCborUrl) return empty;

  const transport = options.transport ?? httpTransport;
  const budget = options.budget ?? sharedBudget;
  const retryBackoffMs = options.retryBackoffMs ?? KOIOS_RETRY_BACKOFF_MS;
  const failureCounts = options.failureCounts ?? {};
  const now = options.now ?? Date.now();

  const pointers: Array<KoiosPointer> = [];
  const transactions: Array<KoiosTransaction> = [];
  const resolutions: Array<AssetResolutionWrite> = [];

  const throttle = (batch: Array<string>) => {
    batch.forEach((subject) =>
      resolutions.push({
        subject,
        state: 'failed',
        failureCount: failureCounts[subject] ?? 0,
        retryAfter: now + KOIOS_THROTTLED_RETRY_MS,
      })
    );
  };

  const fail = (batch: Array<string>) => {
    batch.forEach((subject) => {
      const failureCount = (failureCounts[subject] ?? 0) + 1;
      resolutions.push({
        subject,
        state: 'failed',
        failureCount,
        retryAfter: now + koiosBackoffMs(failureCount),
      });
    });
  };

  /**
   * One batch, and the outcome the caller needs to decide whether to go on.
   * Extracted from the loop because every early exit inside it is a different
   * reason to stop reading this batch, and `continue` is not available here.
   */
  const resolveBatch = async (batch: Array<string>): Promise<Outcome> => {
    const assetList = batch.map((subject) => [
      subject.slice(0, POLICY_ID_HEX_LENGTH),
      subject.slice(POLICY_ID_HEX_LENGTH),
    ]);
    const info = await send(
      assetInfoUrl,
      JSON.stringify({ _asset_list: assetList }),
      transport,
      budget,
      retryBackoffMs,
      now
    );
    if (info.outcome !== 'ok') return info.outcome;

    const requested = new Set(batch);
    const batchPointers: Array<KoiosPointer> = [];
    info.records.forEach((record) => {
      const pointer = toPointer(record, requested);
      if (pointer) batchPointers.push(pointer);
    });

    const hashes = Array.from(
      new Set(batchPointers.map((pointer) => pointer.mintingTxHash))
    );
    // The index answered and knows none of them. Not a failure, and not a row:
    // the registry channel records unregistered subjects, and a second channel
    // doing the same would fight it for the same key.
    if (hashes.length === 0) return 'ok';

    const bytes = await send(
      txCborUrl,
      JSON.stringify({ _tx_hashes: hashes }),
      transport,
      budget,
      retryBackoffMs,
      now
    );
    if (bytes.outcome !== 'ok') return bytes.outcome;

    const wanted = new Set(hashes);
    const batchTransactions: Array<KoiosTransaction> = [];
    bytes.records.forEach((record) => {
      const transaction = toTransaction(record, wanted);
      if (transaction) batchTransactions.push(transaction);
    });

    // A pointer with no bytes behind it cannot be confirmed, so it is not
    // carried forward as if it could be.
    const answered = new Set(
      batchTransactions.map((transaction) => transaction.txHash)
    );
    batchPointers.forEach((pointer) => {
      if (answered.has(pointer.mintingTxHash)) pointers.push(pointer);
    });
    batchTransactions.forEach((transaction) => transactions.push(transaction));
    return 'ok';
  };

  const batches = koiosBatches(distinct);
  // One after another rather than in parallel, so a wallet holding many assets
  // opens one socket at a time and the ceiling is consulted in order.
  for (let index = 0; index < batches.length; index += 1) {
    const outcome = await resolveBatch(batches[index]);
    if (outcome === 'throttled') {
      throttle(batches[index]);
      // The ceiling is a property of the process rather than of this batch, so
      // there is nothing to gain from trying the next one.
      break;
    }
    if (outcome === 'failed') fail(batches[index]);
  }

  return { pointers, transactions, resolutions };
}
