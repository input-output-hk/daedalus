import {
  launcherConfig,
  MOCK_TOKEN_METADATA_SERVER_PORT,
  MOCK_TOKEN_METADATA_SERVER_URL,
} from '../config';
import { environment } from '../environment';
import { logger } from '../utils/logging';
import type { AssetResolutionWrite } from './assetMetadataDb';
import { httpTransport } from './httpTransport';
import type { HttpTransport, HttpTransportResult } from './httpTransport';

export const ASSET_REGISTRY_FALLBACK_URL = 'https://tokens.cardano.org';

/**
 * The `logo` property is deliberately absent. It is inline base64 with a median
 * entry of 36 KiB, and asking for it in bulk is what the per-subject image
 * fetch exists to avoid.
 */
export const ASSET_REGISTRY_PROPERTIES = [
  'name',
  'ticker',
  'decimals',
  'url',
  'description',
];

/**
 * The endpoint caps the request body near 8,192 bytes and does not cap the
 * subject count. Measured against the live endpoint on 2026-09-14: 8,190 bytes
 * returned 200 and 8,682 returned 413, with both requests carrying 86 and 90
 * subjects respectively. A subject is 56 hex characters of policy id plus up to
 * 64 of asset name, so sizing by count fails on asset name length alone.
 */
export const ASSET_REGISTRY_MAX_REQUEST_BYTES = 6 * 1024;

// The largest response measured was 103,314 bytes for 86 subjects with no logo
// requested. This is ANCHOR_MAX_BYTES, reused rather than reinvented.
export const ASSET_REGISTRY_MAX_RESPONSE_BYTES = 1024 * 1024;

// Matches ANCHOR_TIMEOUT_MS. One wall-clock budget per request.
export const ASSET_REGISTRY_TIMEOUT_MS = 10000;

export const ASSET_REGISTRY_RETRY_BACKOFF_MS = 1000;

/**
 * Short enough that a wallet opened during a brief outage recovers within the
 * session, long enough that several hundred permanently unregistered subjects
 * cost one batch a day between them. Chosen rather than measured: no corpus
 * says what the right interval for an unregistered subject is.
 */
export const ASSET_REGISTRY_BACKOFF_BASE_MS = 5 * 60 * 1000;
export const ASSET_REGISTRY_BACKOFF_CEILING_MS = 24 * 60 * 60 * 1000;

export type RegistrySignature = {
  signature: string;
  publicKey: string;
};

export type RegistryProperty = {
  value: unknown;
  sequenceNumber: number;
  signatures: Array<RegistrySignature>;
};

export type RegistryEntry = {
  subject: string;
  policy: string | null;
  properties: Record<string, RegistryProperty>;
};

// Both are the shared transport's, re-exported under the names this module's
// callers and its spec already use.
export type RegistryTransportResult = HttpTransportResult;

export type RegistryTransport = HttpTransport;

export type AssetRegistryQueryOptions = {
  endpoint?: string | null;
  transport?: RegistryTransport;
  failureCounts?: Record<string, number>;
  retryBackoffMs?: number;
  now?: number;
};

export type AssetRegistryQueryResult = {
  entries: Array<RegistryEntry>;
  resolutions: Array<AssetResolutionWrite>;
};

export const assetRegistryEndpoint = (override?: string | null): string => {
  if (override) return override;
  if (launcherConfig.metadataUrl) return launcherConfig.metadataUrl;
  // launcher-config.nix omits metadataUrl on selfnode, so the bare literal
  // below would point a selfnode run at mainnet. The failure is silent: a
  // mainnet registry answers a selfnode query plausibly.
  if (environment.isSelfnode) {
    return `${MOCK_TOKEN_METADATA_SERVER_URL}:${MOCK_TOKEN_METADATA_SERVER_PORT}`;
  }
  return ASSET_REGISTRY_FALLBACK_URL;
};

export const assetRegistryQueryUrl = (endpoint: string): string =>
  `${endpoint.replace(/\/+$/, '')}/metadata/query`;

export const assetRegistryRequestBody = (subjects: Array<string>): string =>
  JSON.stringify({ subjects, properties: ASSET_REGISTRY_PROPERTIES });

export const assetRegistryBackoffMs = (failureCount: number): number => {
  const exponent = Math.max(0, failureCount - 1);
  const scaled = ASSET_REGISTRY_BACKOFF_BASE_MS * 2 ** exponent;
  return Math.min(scaled, ASSET_REGISTRY_BACKOFF_CEILING_MS);
};

export const assetRegistryBatches = (
  subjects: Array<string>
): Array<Array<string>> => {
  const batches: Array<Array<string>> = [];
  const overhead = Buffer.byteLength(assetRegistryRequestBody([]));
  let current: Array<string> = [];
  let bytes = overhead;
  subjects.forEach((subject) => {
    // Measured through JSON.stringify rather than counted, so the arithmetic
    // stays exact for a subject carrying a character JSON escapes.
    const encoded = Buffer.byteLength(JSON.stringify(subject));
    if (
      current.length > 0 &&
      bytes + encoded + 1 > ASSET_REGISTRY_MAX_REQUEST_BYTES
    ) {
      batches.push(current);
      current = [];
      bytes = overhead;
    }
    // The comma is only paid for by a subject that is not first in its batch,
    // which is why the cost is recomputed after the batch may have been closed.
    bytes += encoded + (current.length > 0 ? 1 : 0);
    current.push(subject);
  });
  if (current.length > 0) batches.push(current);
  return batches;
};

const isSignature = (value: unknown): value is RegistrySignature =>
  typeof value === 'object' &&
  value !== null &&
  typeof (value as RegistrySignature).signature === 'string' &&
  typeof (value as RegistrySignature).publicKey === 'string';

const toProperty = (value: unknown): RegistryProperty | null => {
  if (typeof value !== 'object' || value === null) return null;
  const candidate = value as {
    value?: unknown;
    sequenceNumber?: unknown;
    signatures?: unknown;
  };
  if (candidate.value === undefined) return null;
  if (
    typeof candidate.sequenceNumber !== 'number' ||
    !Number.isFinite(candidate.sequenceNumber)
  ) {
    return null;
  }
  if (!Array.isArray(candidate.signatures)) return null;
  if (!candidate.signatures.every(isSignature)) return null;
  return {
    value: candidate.value,
    sequenceNumber: candidate.sequenceNumber,
    signatures: candidate.signatures as Array<RegistrySignature>,
  };
};

const toEntry = (
  value: unknown,
  requested: Set<string>
): RegistryEntry | null => {
  if (typeof value !== 'object' || value === null) return null;
  const candidate = value as Record<string, unknown>;
  const subject = candidate.subject;
  if (typeof subject !== 'string' || !requested.has(subject)) {
    // A server that answers a question nobody asked must not create a row.
    logger.debug('Asset registry: response entry was not requested');
    return null;
  }
  const properties: Record<string, RegistryProperty> = {};
  ASSET_REGISTRY_PROPERTIES.forEach((name) => {
    const property = toProperty(candidate[name]);
    if (property) properties[name] = property;
  });
  return {
    subject,
    policy: typeof candidate.policy === 'string' ? candidate.policy : null,
    properties,
  };
};

const parseEntries = (
  body: string,
  requested: Set<string>
): Array<RegistryEntry> | null => {
  let parsed: unknown;
  try {
    parsed = JSON.parse(body);
  } catch {
    return null;
  }
  const subjects = (parsed as { subjects?: unknown })?.subjects;
  if (!Array.isArray(subjects)) return null;
  const entries: Array<RegistryEntry> = [];
  subjects.forEach((value) => {
    const entry = toEntry(value, requested);
    if (entry) entries.push(entry);
  });
  return entries;
};

/**
 * The registry's transport. `httpTransport` is shared with the pointer client
 * and the name here is kept because every caller in this module and its spec
 * uses it.
 */
export const httpRegistryTransport: RegistryTransport = httpTransport;

const delay = (ms: number): Promise<void> =>
  new Promise((resolve) => {
    if (ms <= 0) {
      resolve();
      return;
    }
    setTimeout(resolve, ms);
  });

type BatchOutcome = {
  entries: Array<RegistryEntry>;
  resolved: Array<string>;
  unregistered: Array<string>;
  failed: Array<string>;
};

const emptyOutcome = (): BatchOutcome => ({
  entries: [],
  resolved: [],
  unregistered: [],
  failed: [],
});

const mergeOutcome = (
  into: BatchOutcome,
  from: BatchOutcome
): BatchOutcome => ({
  entries: into.entries.concat(from.entries),
  resolved: into.resolved.concat(from.resolved),
  unregistered: into.unregistered.concat(from.unregistered),
  failed: into.failed.concat(from.failed),
});

// Narrowed by equality rather than by truthiness. `tsconfig.json` runs with
// `strict: false`, under which `if (!result.ok)` does not narrow a boolean
// literal discriminant while `if (result.ok === false)` does.
const isRetryable = (result: RegistryTransportResult): boolean => {
  if (result.ok === false) return result.reason !== 'too-large';
  return result.status >= 500;
};

async function sendBatch(
  subjects: Array<string>,
  url: string,
  transport: RegistryTransport,
  retryBackoffMs: number,
  maySplit: boolean
): Promise<BatchOutcome> {
  const body = assetRegistryRequestBody(subjects);
  let result = await transport.post(
    url,
    body,
    ASSET_REGISTRY_TIMEOUT_MS,
    ASSET_REGISTRY_MAX_RESPONSE_BYTES
  );
  if (isRetryable(result)) {
    await delay(retryBackoffMs);
    result = await transport.post(
      url,
      body,
      ASSET_REGISTRY_TIMEOUT_MS,
      ASSET_REGISTRY_MAX_RESPONSE_BYTES
    );
  }

  if (result.ok === false) {
    logger.debug('Asset registry: batch abandoned', {
      reason: result.reason,
      subjectCount: subjects.length,
    });
    return { ...emptyOutcome(), failed: subjects };
  }

  if (result.status === 413 && maySplit && subjects.length > 1) {
    const middle = Math.floor(subjects.length / 2);
    const first = await sendBatch(
      subjects.slice(0, middle),
      url,
      transport,
      retryBackoffMs,
      false
    );
    const second = await sendBatch(
      subjects.slice(middle),
      url,
      transport,
      retryBackoffMs,
      false
    );
    return mergeOutcome(first, second);
  }

  if (result.status < 200 || result.status >= 300) {
    if (result.status >= 400 && result.status < 500) {
      // Deterministic. Retrying a request the endpoint refuses produces the
      // same refusal forever, and a doubling backoff would turn a fixable
      // sizing error into subjects that are never resolved and never
      // diagnosed.
      logger.warn('Asset registry: request refused', {
        status: result.status,
        requestBytes: Buffer.byteLength(body),
        subjectCount: subjects.length,
      });
    } else {
      logger.debug('Asset registry: batch abandoned', {
        status: result.status,
        subjectCount: subjects.length,
      });
    }
    return { ...emptyOutcome(), failed: subjects };
  }

  const requested = new Set(subjects);
  const entries = parseEntries(result.body, requested);
  if (entries === null) {
    logger.debug('Asset registry: response could not be read', {
      subjectCount: subjects.length,
    });
    return { ...emptyOutcome(), failed: subjects };
  }

  const answered = new Set(entries.map((entry) => entry.subject));
  return {
    entries,
    resolved: subjects.filter((subject) => answered.has(subject)),
    // The registry omits what it does not know rather than returning a
    // negative, so a short response is a success. Recording the omission is
    // what stops every demand re-scheduling the same subject.
    unregistered: subjects.filter((subject) => !answered.has(subject)),
    failed: [],
  };
}

export async function queryAssetRegistry(
  subjects: Array<string>,
  options: AssetRegistryQueryOptions = {}
): Promise<AssetRegistryQueryResult> {
  const distinct = Array.from(new Set(subjects));
  if (distinct.length === 0) return { entries: [], resolutions: [] };

  const transport = options.transport ?? httpRegistryTransport;
  const url = assetRegistryQueryUrl(assetRegistryEndpoint(options.endpoint));
  const retryBackoffMs =
    options.retryBackoffMs ?? ASSET_REGISTRY_RETRY_BACKOFF_MS;
  const failureCounts = options.failureCounts ?? {};
  const now = options.now ?? Date.now();

  let outcome = emptyOutcome();
  const batches = assetRegistryBatches(distinct);
  // One after another rather than in parallel, so a wallet with many tokens
  // opens one socket at a time.
  for (let index = 0; index < batches.length; index += 1) {
    const batch = await sendBatch(
      batches[index],
      url,
      transport,
      retryBackoffMs,
      true
    );
    outcome = mergeOutcome(outcome, batch);
  }

  const backoffRow = (
    subject: string,
    state: AssetResolutionWrite['state']
  ): AssetResolutionWrite => {
    const failureCount = (failureCounts[subject] ?? 0) + 1;
    return {
      subject,
      state,
      failureCount,
      retryAfter: now + assetRegistryBackoffMs(failureCount),
    };
  };

  return {
    entries: outcome.entries,
    resolutions: [
      ...outcome.resolved.map(
        (subject): AssetResolutionWrite => ({
          subject,
          state: 'resolved',
          failureCount: 0,
          retryAfter: 0,
        })
      ),
      ...outcome.unregistered.map((subject) =>
        backoffRow(subject, 'unregistered')
      ),
      ...outcome.failed.map((subject) => backoffRow(subject, 'failed')),
    ],
  };
}
