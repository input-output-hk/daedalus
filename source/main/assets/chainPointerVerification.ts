import blake2b from 'blake2b';
import { logger } from '../utils/logging';
import {
  CborError,
  arraySpans,
  mapSpans,
  readBytes,
  readHead,
  readInteger,
  readText,
  unwrapTag,
} from './cborSpan';
import type { CborSpan } from './cborSpan';
import { ImmutableBlockReader } from './immutableBlockReader';
import type { ImmutableReadResult } from './immutableBlockReader';

/**
 * Koios is read as an index, not as an oracle.
 *
 * Three checks run against the bytes it returned, with no chain access at all:
 * the transaction body hashes to the transaction id it claims, the auxiliary
 * data hashes to the value in transaction body key 7, and body key 9 mints the
 * subject under the subject's own policy. Together those say the bytes are
 * self-consistent and that the metadata is bound to that transaction by its
 * hash.
 *
 * They do not say the transaction was ever accepted into a block. Anyone can
 * construct bytes that never were. The fourth check is what closes that: the
 * block at the pointer is read out of the user's own immutable database and one
 * of its transaction bodies has to hash to the same transaction id.
 *
 * A pointer newer than the immutable database is not a failure. The last k
 * blocks live in `volatile/`, which is a different on-disk format and is
 * deliberately out of scope, so such a pointer is unresolved for now and is
 * looked at again later.
 */

const DIGEST_BYTES = 32;
const POLICY_ID_HEX_LENGTH = 56;

/** CIP-25 payloads live under metadata label 721. */
export const CIP25_METADATA_LABEL = 721;

/** Alonzo onwards wraps auxiliary data in this tag. */
const AUXILIARY_DATA_TAG = 259;

export type PointerConfirmation =
  | {
      status: 'confirmed';
      subject: string;
      slot: number;
      cip25: Record<string, unknown> | null;
    }
  /** Newer than the immutable database. Try again once it has caught up. */
  | { status: 'pending'; reason: 'beyond-immutable-tip'; tipSlot: number }
  /** The bytes or the pointer are wrong. No row, ever, for this pointer. */
  | { status: 'rejected'; reason: string }
  /** Nothing could be decided. No row, and no conclusion about the pointer. */
  | { status: 'unavailable'; reason: string };

export type PointerCandidate = {
  subject: string;
  policyId: string;
  assetName: string;
  txHash: string;
  blockHash: string;
  absoluteSlot: number;
  /** The raw transaction, hex, exactly as the index returned it. */
  cbor: string;
};

const digest = (input: Uint8Array): string =>
  Buffer.from(
    blake2b(DIGEST_BYTES)
      // blake2b guards its input with a realm-sensitive `instanceof Uint8Array`,
      // so a Buffer from another realm is rejected. `Uint8Array.from` is the
      // same guard the registry verification uses.
      .update(Uint8Array.from(input))
      .digest()
  ).toString('hex');

const hex = (input: Uint8Array): string => Buffer.from(input).toString('hex');

const slice = (bytes: Uint8Array, span: CborSpan): Uint8Array =>
  bytes.subarray(span.start, span.end);

const isHex = (value: string, length?: number): boolean =>
  /^[0-9a-f]*$/i.test(value) &&
  value.length % 2 === 0 &&
  (length === undefined || value.length === length);

/**
 * The entries of a transaction body, keyed by its integer keys.
 *
 * A body is a map whose keys are small unsigned integers. Anything else in key
 * position is a body this code does not understand, and the caller refuses it
 * rather than skipping the key.
 */
const bodyEntries = (
  bytes: Uint8Array,
  span: CborSpan
): Map<number, CborSpan> => {
  const entries = new Map<number, CborSpan>();
  mapSpans(bytes, span.start).forEach((entry) => {
    const head = readHead(bytes, entry.key.start);
    if (head.major !== 0) throw new CborError('transaction body key');
    entries.set(Number(head.argument), entry.value);
  });
  return entries;
};

/**
 * Whether the mint field mints this subject under this policy, in a positive
 * quantity.
 *
 * The quantity matters. A negative quantity is a burn, and a transaction that
 * burns an asset is not the transaction that named it.
 */
const mintsSubject = (
  bytes: Uint8Array,
  span: CborSpan,
  policyId: string,
  assetName: string
): boolean =>
  mapSpans(bytes, span.start).some((policyEntry) => {
    if (hex(readBytes(bytes, policyEntry.key.start)) !== policyId) return false;
    return mapSpans(bytes, policyEntry.value.start).some((assetEntry) => {
      if (hex(readBytes(bytes, assetEntry.key.start)) !== assetName) {
        return false;
      }
      return readInteger(bytes, assetEntry.value.start) > BigInt(0);
    });
  });

/**
 * The metadata map inside auxiliary data, whichever of its three historical
 * shapes it is in.
 *
 * Shelley: a bare metadata map. Allegra and Mary: a two-element array of the
 * metadata map and the scripts. Alonzo onwards: a tag 259 map whose key 0 holds
 * the metadata. Returning null means the shape is one this code does not read,
 * not that there is no metadata.
 */
const metadataSpan = (bytes: Uint8Array, span: CborSpan): CborSpan | null => {
  const head = readHead(bytes, span.start);
  if (head.major === 6) {
    const { tag, span: inner } = unwrapTag(bytes, span.start);
    if (tag !== BigInt(AUXILIARY_DATA_TAG)) return null;
    const entry = mapSpans(bytes, inner.start).find((candidate) => {
      const key = readHead(bytes, candidate.key.start);
      return key.major === 0 && key.argument === BigInt(0);
    });
    return entry ? entry.value : null;
  }
  if (head.major === 4) {
    const elements = arraySpans(bytes, span.start);
    return elements.length > 0 ? elements[0] : null;
  }
  if (head.major === 5) return span;
  return null;
};

/**
 * A metadatum, as JSON.
 *
 * CIP-25 payloads are maps of text to text, arrays and further maps, and the
 * consumer of this is a name on a token row. A key that is not text becomes its
 * hex, because a JSON object cannot hold a byte string as a key and dropping it
 * would silently change the record.
 */
const metadatumToJson = (bytes: Uint8Array, span: CborSpan): unknown => {
  const head = readHead(bytes, span.start);
  switch (head.major) {
    case 0:
    case 1:
      // A metadatum integer is bounded by the ledger rules to something a
      // JavaScript number holds, and this value is rendered rather than
      // computed with.
      return Number(readInteger(bytes, span.start));
    case 2:
      return hex(readBytes(bytes, span.start));
    case 3:
      return readText(bytes, span.start);
    case 4:
      return arraySpans(bytes, span.start).map((element) =>
        metadatumToJson(bytes, element)
      );
    case 5: {
      const object: Record<string, unknown> = {};
      mapSpans(bytes, span.start).forEach((entry) => {
        const keyHead = readHead(bytes, entry.key.start);
        const key =
          keyHead.major === 3
            ? readText(bytes, entry.key.start)
            : String(metadatumToJson(bytes, entry.key));
        object[key] = metadatumToJson(bytes, entry.value);
      });
      return object;
    }
    default:
      return null;
  }
};

/**
 * The CIP-25 record for one subject, from the auxiliary data of the transaction
 * that minted it.
 *
 * The payload is `{ 721: { <policy>: { <assetName>: { ... } } } }`, with the
 * asset name as text rather than as hex. CIP-25 version 2 uses byte strings for
 * both, so both spellings are looked for.
 */
const cip25For = (
  bytes: Uint8Array,
  auxiliary: CborSpan,
  policyId: string,
  assetName: string
): Record<string, unknown> | null => {
  const metadata = metadataSpan(bytes, auxiliary);
  if (!metadata) return null;
  const labelled = mapSpans(bytes, metadata.start).find((entry) => {
    const head = readHead(bytes, entry.key.start);
    return head.major === 0 && head.argument === BigInt(CIP25_METADATA_LABEL);
  });
  if (!labelled) return null;
  const payload = metadatumToJson(bytes, labelled.value);
  if (!payload || typeof payload !== 'object') return null;

  const nameText = (() => {
    try {
      return Buffer.from(assetName, 'hex').toString('utf8');
    } catch {
      return null;
    }
  })();

  const byPolicy = (payload as Record<string, unknown>)[policyId];
  const policyMap =
    byPolicy && typeof byPolicy === 'object'
      ? (byPolicy as Record<string, unknown>)
      : null;
  if (!policyMap) return null;

  const candidates = [assetName, nameText].filter(
    (candidate): candidate is string => typeof candidate === 'string'
  );
  const found = candidates
    .map((candidate) => policyMap[candidate])
    .find((value) => value && typeof value === 'object');
  return (found as Record<string, unknown>) ?? null;
};

/**
 * Whether the block holds a transaction with this id.
 *
 * A Cardano block on disk is `[eraTag, block]`, and from Shelley onwards the
 * block is an array whose second element is the transaction bodies. Each body
 * is hashed as it appears, which is what a transaction id is. Byron blocks have
 * a different shape entirely and hold no native assets, so a block whose second
 * element is not an array of bodies is refused rather than searched.
 */
const blockHoldsTransaction = (block: Uint8Array, txHash: string): boolean => {
  const outer = arraySpans(block, 0);
  if (outer.length !== 2) throw new CborError('block is not an era wrapper');
  const inner = arraySpans(block, outer[1].start);
  if (inner.length < 4) throw new CborError('block has too few parts');
  return arraySpans(block, inner[1].start).some(
    (body) => digest(slice(block, body)) === txHash
  );
};

export type ConfirmPointerOptions = {
  reader: ImmutableBlockReader;
};

/**
 * Confirms one pointer, or says why it could not be.
 *
 * Every failing path returns without a confirmation, and the caller writes no
 * row on any of them. The three outcomes that are not `confirmed` differ in what
 * the caller should do next, not in whether a row is written.
 */
export const confirmChainPointer = (
  candidate: PointerCandidate,
  options: ConfirmPointerOptions
): PointerConfirmation => {
  const { subject, policyId, assetName, txHash, blockHash, absoluteSlot } =
    candidate;

  if (!isHex(txHash, 64) || !isHex(blockHash, 64)) {
    return { status: 'rejected', reason: 'pointer-not-hex' };
  }
  if (!isHex(policyId, POLICY_ID_HEX_LENGTH) || !isHex(assetName)) {
    return { status: 'rejected', reason: 'subject-not-hex' };
  }
  if (subject !== `${policyId}${assetName}`) {
    return { status: 'rejected', reason: 'subject-mismatch' };
  }

  let bytes: Uint8Array;
  try {
    bytes = Uint8Array.from(Buffer.from(candidate.cbor, 'hex'));
  } catch {
    return { status: 'rejected', reason: 'transaction-not-hex' };
  }
  if (bytes.length === 0) {
    return { status: 'rejected', reason: 'transaction-empty' };
  }

  let cip25: Record<string, unknown> | null = null;
  try {
    const parts = arraySpans(bytes, 0);
    if (parts.length < 2) {
      return { status: 'rejected', reason: 'transaction-shape' };
    }

    // 1. The body hashes to the transaction id the index claims.
    const body = parts[0];
    if (digest(slice(bytes, body)) !== txHash.toLowerCase()) {
      return { status: 'rejected', reason: 'transaction-id' };
    }

    const entries = bodyEntries(bytes, body);

    // 2. The mint field names this subject, positively.
    const mint = entries.get(9);
    if (!mint) return { status: 'rejected', reason: 'no-mint-field' };
    if (
      !mintsSubject(
        bytes,
        mint,
        policyId.toLowerCase(),
        assetName.toLowerCase()
      )
    ) {
      return { status: 'rejected', reason: 'mint-does-not-name-subject' };
    }

    // 3. The auxiliary data hashes to body key 7. A transaction with no
    // auxiliary data must not claim one, and one that claims a hash must carry
    // data matching it.
    const declared = entries.get(7);
    // Alonzo onwards is `[body, witnesses, isValid, auxiliary]`; Mary and
    // earlier are `[body, witnesses, auxiliary]`. In both the auxiliary data is
    // last, and `null` there means there is none.
    const auxiliary = parts[parts.length - 1];
    const auxiliaryHead = readHead(bytes, auxiliary.start);
    const auxiliaryIsNull =
      parts.length < 3 ||
      (auxiliaryHead.major === 7 && auxiliaryHead.additional === 22);

    if (declared && auxiliaryIsNull) {
      return { status: 'rejected', reason: 'auxiliary-data-missing' };
    }
    if (!declared && !auxiliaryIsNull) {
      return { status: 'rejected', reason: 'auxiliary-data-unclaimed' };
    }
    if (declared && !auxiliaryIsNull) {
      const claimed = hex(readBytes(bytes, declared.start));
      if (digest(slice(bytes, auxiliary)) !== claimed) {
        return { status: 'rejected', reason: 'auxiliary-data-hash' };
      }
      cip25 = cip25For(
        bytes,
        auxiliary,
        policyId.toLowerCase(),
        assetName.toLowerCase()
      );
    }
  } catch (error) {
    return {
      status: 'rejected',
      reason: error instanceof CborError ? `cbor:${error.message}` : 'cbor',
    };
  }

  // 4. The transaction is in the block the pointer names, in the user's own
  // chain. Nothing above establishes that it was ever accepted.
  const read: ImmutableReadResult = options.reader.readBlock(
    absoluteSlot,
    blockHash
  );
  if (read.status === 'beyond-immutable-tip') {
    return {
      status: 'pending',
      reason: 'beyond-immutable-tip',
      tipSlot: read.tipSlot,
    };
  }
  if (read.status === 'unreadable') {
    logger.debug('Chain pointer: immutable database could not be read', {
      reason: read.reason,
    });
    return { status: 'unavailable', reason: read.reason };
  }
  if (read.status === 'absent') {
    return { status: 'rejected', reason: 'block-not-in-chain' };
  }

  try {
    if (!blockHoldsTransaction(read.bytes, txHash.toLowerCase())) {
      return { status: 'rejected', reason: 'transaction-not-in-block' };
    }
  } catch (error) {
    // A block shape this reader does not recognise fails closed rather than
    // being taken as a confirmation or as a rejection of the pointer.
    logger.debug('Chain pointer: block shape not recognised', {
      reason: error instanceof Error ? error.message : 'unknown',
    });
    return { status: 'unavailable', reason: 'block-shape' };
  }

  return { status: 'confirmed', subject, slot: absoluteSlot, cip25 };
};
