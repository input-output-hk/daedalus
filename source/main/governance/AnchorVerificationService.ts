import blake2b from 'blake2b';
import { AnchorFetchErrorType } from '../../common/types/governance.types';
import type {
  DRepAnchorPresence,
  DRepAnchorResult,
  VerifiedDRepAnchorContent,
  VerifiedDRepReference,
  VerifiedDRepReferenceType,
} from '../../common/types/governance.types';
import { fetchAnchorBytes } from './AnchorFetchService';
import {
  deleteVerifiedAnchorBytes,
  isValidAnchorHash,
  readVerifiedAnchorBytes,
  writeVerifiedAnchorBytes,
} from './anchorCache';

const ANCHOR_DIGEST_BYTES = 32;
// CIP-119 caps only givenName. The other bounds are rendering-safety limits so
// one hostile anchor cannot produce an unbounded detail view; the transport's
// body cap bounds the aggregate, these bound the individual field.
const GIVEN_NAME_MAX_LENGTH = 80;
const PROSE_MAX_LENGTH = 1000;
const REFERENCE_LABEL_MAX_LENGTH = 200;
const REFERENCE_URI_MAX_LENGTH = 2048;
const MAX_REFERENCES = 20;
const PAYMENT_ADDRESS_MAX_LENGTH = 128;

const inFlightByHash = new Map<string, Promise<DRepAnchorResult>>();

export const anchorDigest = (bytes: Buffer): string =>
  blake2b(ANCHOR_DIGEST_BYTES).update(bytes).digest('hex');

const unavailable = (reason: AnchorFetchErrorType): DRepAnchorResult => ({
  status: 'unavailable',
  reason,
});

function readCip119Raw(raw: unknown): string | null {
  let value: string | null = null;
  if (typeof raw === 'string') {
    value = raw;
  } else if (raw !== null && typeof raw === 'object') {
    const wrapped = (raw as Record<string, unknown>)['@value'];
    if (typeof wrapped === 'string') value = wrapped;
  }
  if (value === null) return null;
  const trimmed = value.trim();
  return trimmed === '' ? null : trimmed;
}

function readCip119String(raw: unknown, maxLength: number): string | null {
  const value = readCip119Raw(raw);
  return value === null ? null : value.slice(0, maxLength);
}

// The address is rendered with a copy button, so an over-length value is dropped
// rather than clamped: a truncated address a user can copy is worse than none.
function readPaymentAddress(raw: unknown): string | null {
  const value = readCip119Raw(raw);
  if (value === null) return null;
  return value.length > PAYMENT_ADDRESS_MAX_LENGTH ? null : value;
}

function readCip119Boolean(raw: unknown): boolean {
  if (typeof raw === 'boolean') return raw;
  if (raw !== null && typeof raw === 'object') {
    const wrapped = (raw as Record<string, unknown>)['@value'];
    if (typeof wrapped === 'boolean') return wrapped;
  }
  return false;
}

// An Identity claim outranks a Link claim so the cautionary sub-section wins
// whenever an anchor asserts both; anything unrecognised collapses to 'other',
// which the renderer treats as a plain link and never as a claimed identity.
function readReferenceType(raw: unknown): VerifiedDRepReferenceType {
  const candidates = Array.isArray(raw) ? raw : [raw];
  let sawLink = false;
  for (const candidate of candidates) {
    const value = readCip119Raw(candidate);
    if (value !== null) {
      const localName = value.toLowerCase().split(/[:/#]/).pop() ?? '';
      if (localName === 'identity') return 'identity';
      if (localName === 'link') sawLink = true;
    }
  }
  return sawLink ? 'link' : 'other';
}

function readCip119References(raw: unknown): VerifiedDRepReference[] {
  if (!Array.isArray(raw)) return [];
  const references: VerifiedDRepReference[] = [];
  for (const item of raw) {
    if (references.length >= MAX_REFERENCES) break;
    if (item !== null && typeof item === 'object' && !Array.isArray(item)) {
      const record = item as Record<string, unknown>;
      const uri = readCip119Raw(record.uri);
      if (uri !== null && uri.length <= REFERENCE_URI_MAX_LENGTH) {
        references.push({
          type: readReferenceType(record['@type']),
          label: readCip119String(record.label, REFERENCE_LABEL_MAX_LENGTH),
          uri,
        });
      }
    }
  }
  return references;
}

// Every CIP-119 field is optional. A document that omits givenName still carries
// doNotList and the profile fields, so it parses; each renderer guards its own
// field rather than the whole document failing on one absent value.
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
  const fields = body as Record<string, unknown>;
  return {
    givenName: readCip119String(fields.givenName, GIVEN_NAME_MAX_LENGTH),
    objectives: readCip119String(fields.objectives, PROSE_MAX_LENGTH),
    motivations: readCip119String(fields.motivations, PROSE_MAX_LENGTH),
    qualifications: readCip119String(fields.qualifications, PROSE_MAX_LENGTH),
    references: readCip119References(fields.references),
    paymentAddress: readPaymentAddress(fields.paymentAddress),
    doNotList: readCip119Boolean(fields.doNotList),
  };
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
