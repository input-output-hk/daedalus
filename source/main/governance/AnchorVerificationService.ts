import blake2b from 'blake2b';
import { AnchorFetchErrorType } from '../../common/types/governance.types';
import type {
  DRepAnchorPresence,
  DRepAnchorResult,
  VerifiedDRepAnchorContent,
} from '../../common/types/governance.types';
import { fetchAnchorBytes } from './AnchorFetchService';
import {
  deleteVerifiedAnchorBytes,
  isValidAnchorHash,
  readVerifiedAnchorBytes,
  writeVerifiedAnchorBytes,
} from './anchorCache';

const ANCHOR_DIGEST_BYTES = 32;
const GIVEN_NAME_MAX_LENGTH = 80;

const inFlightByHash = new Map<string, Promise<DRepAnchorResult>>();

export const anchorDigest = (bytes: Buffer): string =>
  blake2b(ANCHOR_DIGEST_BYTES).update(bytes).digest('hex');

const unavailable = (reason: AnchorFetchErrorType): DRepAnchorResult => ({
  status: 'unavailable',
  reason,
});

function readCip119String(raw: unknown): string | null {
  let value: string | null = null;
  if (typeof raw === 'string') {
    value = raw;
  } else if (raw !== null && typeof raw === 'object') {
    const wrapped = (raw as Record<string, unknown>)['@value'];
    if (typeof wrapped === 'string') value = wrapped;
  }
  if (value === null) return null;
  const trimmed = value.trim();
  if (trimmed === '') return null;
  return trimmed.slice(0, GIVEN_NAME_MAX_LENGTH);
}

function parseVerifiedContent(bytes: Buffer): VerifiedDRepAnchorContent | null {
  let parsed: unknown;
  try {
    parsed = JSON.parse(bytes.toString('utf8'));
  } catch {
    return null;
  }
  if (parsed === null || typeof parsed !== 'object' || Array.isArray(parsed)) {
    return null;
  }
  const body = (parsed as Record<string, unknown>).body;
  if (body === null || typeof body !== 'object' || Array.isArray(body)) {
    return null;
  }
  const givenName = readCip119String(
    (body as Record<string, unknown>).givenName
  );
  if (givenName === null) return null;
  return { givenName };
}

// Fetch, verify, parse, cache, respond. The digest check gates every step that
// follows it: unverified bytes never reach JSON.parse and never reach the cache.
async function resolveFromCacheOrFetch(
  url: string,
  hash: string,
  host: string
): Promise<DRepAnchorResult> {
  const cached = readVerifiedAnchorBytes(hash);
  if (cached !== null) {
    if (anchorDigest(cached) === hash) {
      const content = parseVerifiedContent(cached);
      if (content === null)
        return unavailable(AnchorFetchErrorType.ParseFailed);
      return { status: 'verified', content, host, fetchedAt: Date.now() };
    }
    deleteVerifiedAnchorBytes(hash);
  }

  const fetched = await fetchAnchorBytes(url);
  if (fetched.ok === false) return unavailable(fetched.reason);

  if (anchorDigest(fetched.bytes) !== hash) {
    return unavailable(AnchorFetchErrorType.HashMismatch);
  }

  writeVerifiedAnchorBytes(hash, fetched.bytes);

  const content = parseVerifiedContent(fetched.bytes);
  if (content === null) return unavailable(AnchorFetchErrorType.ParseFailed);

  return { status: 'verified', content, host, fetchedAt: Date.now() };
}

export function resolveVerifiedAnchor(
  anchor: DRepAnchorPresence
): Promise<DRepAnchorResult> {
  const hash =
    typeof anchor?.hash === 'string' ? anchor.hash.trim().toLowerCase() : '';
  const url = typeof anchor?.url === 'string' ? anchor.url.trim() : '';
  if (!isValidAnchorHash(hash) || url === '') {
    return Promise.resolve(unavailable(AnchorFetchErrorType.InvalidRequest));
  }

  let host: string;
  try {
    host = new URL(url).hostname;
  } catch {
    return Promise.resolve(unavailable(AnchorFetchErrorType.InvalidRequest));
  }

  const inFlight = inFlightByHash.get(hash);
  if (inFlight) return inFlight;

  const pending = resolveFromCacheOrFetch(url, hash, host).finally(() => {
    inFlightByHash.delete(hash);
  });
  inFlightByHash.set(hash, pending);
  return pending;
}
