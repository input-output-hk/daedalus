import blake2b from 'blake2b';
import { AnchorFetchErrorType } from '../../common/types/governance.types';
import type {
  DRepAdditionalField,
  DRepAdditionalValue,
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

/**
 * Two kinds of bound, and only one of them belongs here.
 *
 * Volume is bounded once, at the transport, by ANCHOR_MAX_BYTES. That single
 * cap already limits every field, every reference and their sum, and it is the
 * only place a limit can be enforced without having read the document.
 *
 * What remains here is validity, not volume. `givenName` has a length in
 * CIP-119, and a payment address has one because a bech32 Cardano address
 * cannot be longer than that and a longer string is therefore not an address.
 * Those are checks on whether a value is the thing it claims to be.
 *
 * The prose fields, the reference labels and the reference count previously
 * carried limits of their own, justified as rendering safety. They were the
 * wrong tool twice over. They clamped rather than rejected, so a DRep who wrote
 * at length was silently cut off mid-sentence with nothing on screen to say so;
 * and they clamped at parse time, which destroys the text before the cache and
 * the renderer ever see it, leaving no way to offer the rest later. How much of
 * a long field to show at once is a question for the view, which can collapse
 * and expand. It is not a reason to throw the data away.
 */
const GIVEN_NAME_MAX_LENGTH = 80;
// A bech32 mainnet base address is 103 characters; anything longer is not one.
const PAYMENT_ADDRESS_MAX_LENGTH = 128;
// The de facto interoperable URL length. Beyond it a link is unlikely to
// survive the browser it is handed to, so a longer value is dropped rather than
// offered as something to click.
const REFERENCE_URI_MAX_LENGTH = 2048;

/**
 * Keys the canonical block already renders, plus the JSON-LD scaffolding and
 * the terms deliberately withheld. Everything not listed here is carried
 * through as an additional field.
 *
 * `image` is withheld rather than unrecognised. A linked one discloses the
 * reader's address to a host the DRep chose, at page load and with no click,
 * and 239 of the 404 mainnet DReps with metadata supply one. An inlined one
 * trades that for weight: the largest document in the sample is 274,310 bytes,
 * of which 268,625 is a single base64 JPEG. Neither form is shown, so neither
 * is carried.
 *
 * `dRepName` is dropped as a duplicate. It appears on 100 sampled DReps and in
 * all 100 it repeats `givenName` exactly, so surfacing it would show the same
 * name twice under two headings.
 */
const CANONICAL_KEYS: ReadonlySet<string> = new Set([
  'givenName',
  'objectives',
  'motivations',
  'qualifications',
  'paymentAddress',
  'references',
  'doNotList',
]);

const WITHHELD_KEYS: ReadonlySet<string> = new Set([
  'image',
  'logo',
  'dRepName',
]);

// JSON-LD structure rather than content.
const STRUCTURAL_KEY_PREFIX = '@';

const DATA_URI_PATTERN = /^\s*data:/i;

/** Any inlined payload, whatever key it arrived under. */
function isDataUri(value: string): boolean {
  return DATA_URI_PATTERN.test(value);
}

/**
 * How deep the reader will follow a document's own structure.
 *
 * A structural guard rather than a limit on how much a DRep may say: the
 * renderer walks this tree recursively, and a document nesting ten thousand
 * levels deep would exhaust the stack rather than produce a page. Six is far
 * past anything a profile needs, a members list with fields inside each member
 * reaching three.
 */
const MAX_ADDITIONAL_DEPTH = 6;

function readAdditionalValue(
  raw: unknown,
  depth: number
): DRepAdditionalValue | null {
  if (depth > MAX_ADDITIONAL_DEPTH) return null;

  if (typeof raw === 'number' || typeof raw === 'boolean') {
    return { kind: 'text', text: String(raw) };
  }

  const text = readCip119String(raw);
  if (text !== null) {
    if (text.trim() === '') return null;
    // Banning images by key name only bans the keys we thought of. A data URI
    // is the same payload whatever it arrived under: it carries nothing a
    // reader can read, and the largest in the mainnet sample is 268,625 bytes
    // of base64 that would render as a wall of characters.
    if (isDataUri(text)) return null;
    return { kind: 'text', text };
  }

  if (Array.isArray(raw)) {
    const items = raw
      .map((item) => readAdditionalValue(item, depth + 1))
      .filter((item): item is DRepAdditionalValue => item !== null);
    return items.length > 0 ? { kind: 'list', items } : null;
  }

  if (raw !== null && typeof raw === 'object') {
    const fields = readAdditionalFields(
      raw as Record<string, unknown>,
      depth + 1
    );
    return fields.length > 0 ? { kind: 'group', fields } : null;
  }

  return null;
}

/**
 * Everything the document carried that the canonical block does not.
 *
 * Structure is kept rather than flattened or discarded. A multi-sig DRep that
 * publishes its members, each with a name and a title, has written something a
 * reader wants; turning it into one string would lose which name went with
 * which title, and dropping it would lose the members entirely. Every leaf is
 * text and nothing here becomes clickable, so keeping the shape costs nothing
 * that keeping the strings does not already cost.
 */
function readAdditionalFields(
  fields: Record<string, unknown>,
  depth = 0
): DRepAdditionalField[] {
  const additional: DRepAdditionalField[] = [];
  Object.keys(fields).forEach((key) => {
    if (key.startsWith(STRUCTURAL_KEY_PREFIX)) return;
    // Only the top level holds canonical fields; nested keys are the author's.
    if (depth === 0 && (CANONICAL_KEYS.has(key) || WITHHELD_KEYS.has(key))) {
      return;
    }
    const value = readAdditionalValue(fields[key], depth);
    if (value === null) return;
    additional.push({ key, value });
  });
  return additional;
}

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

function readCip119String(raw: unknown, maxLength?: number): string | null {
  const value = readCip119Raw(raw);
  if (value === null) return null;
  return maxLength === undefined ? value : value.slice(0, maxLength);
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
    if (item !== null && typeof item === 'object' && !Array.isArray(item)) {
      const record = item as Record<string, unknown>;
      const uri = readCip119Raw(record.uri);
      if (uri !== null && uri.length <= REFERENCE_URI_MAX_LENGTH) {
        references.push({
          type: readReferenceType(record['@type']),
          label: readCip119String(record.label),
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
    objectives: readCip119String(fields.objectives),
    motivations: readCip119String(fields.motivations),
    qualifications: readCip119String(fields.qualifications),
    references: readCip119References(fields.references),
    paymentAddress: readPaymentAddress(fields.paymentAddress),
    doNotList: readCip119Boolean(fields.doNotList),
    additionalFields: readAdditionalFields(fields),
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
