/**
 * A structural CBOR reader that hands back the byte range of a decoded item.
 *
 * **Why this exists rather than a library.** A Cardano transaction id is
 * blake2b-256 over the transaction body **as it was originally encoded**, and
 * the auxiliary-data hash in body key 7 is blake2b-256 over the auxiliary data
 * as it was originally encoded. Re-encoding either and hashing the result is
 * wrong for any transaction whose encoder did not produce the same bytes a
 * canonical encoder would, and there is no way to tell from the decoded value
 * whether that is the case. Both `cbor@5.0.2` and `borc@2.1.2` are decoders that
 * return values; neither exposes where in the input a decoded item started and
 * ended.
 *
 * The alternative was a new runtime dependency, which this branch does not add.
 * What is here instead is deliberately not a decoder: it walks the structure,
 * measures it, and returns offsets. Values are read from those offsets by the
 * caller, one type at a time.
 *
 * **Why writing it is defensible.** A parser that gets a boundary wrong produces
 * a hash that does not match, and every use of this module is inside a check
 * whose failure means no row is written. A defect here cannot admit a bad
 * record; it can only fail to admit a good one.
 *
 * Every read is bounds-checked and nesting is capped, because the bytes come
 * from a third-party index.
 */

export class CborError extends Error {}

export const CBOR_MAX_DEPTH = 128;

export type CborSpan = {
  start: number;
  end: number;
};

export type CborHead = {
  major: number;
  additional: number;
  /** The head's argument. A length for strings and containers, a value for integers. */
  argument: bigint;
  indefinite: boolean;
  /** Offset of the first byte after the head. */
  payload: number;
};

const MAJOR_UNSIGNED = 0;
const MAJOR_NEGATIVE = 1;
const MAJOR_BYTES = 2;
const MAJOR_TEXT = 3;
const MAJOR_ARRAY = 4;
const MAJOR_MAP = 5;
const MAJOR_TAG = 6;
const MAJOR_SIMPLE = 7;

const BREAK = 0xff;

const ZERO = BigInt(0);
const ONE = BigInt(1);
const EIGHT = BigInt(8);

const need = (bytes: Uint8Array, offset: number, count: number): void => {
  if (offset < 0 || offset + count > bytes.length) {
    throw new CborError('truncated');
  }
};

export const readHead = (bytes: Uint8Array, offset: number): CborHead => {
  need(bytes, offset, 1);
  const initial = bytes[offset];
  const major = initial >> 5;
  const additional = initial & 0x1f;
  let cursor = offset + 1;
  // `BigInt(0)` rather than `0n`: `tsconfig.json` targets es2019 and bigint
  // literals need es2020.
  let argument = ZERO;
  let indefinite = false;

  if (additional < 24) {
    argument = BigInt(additional);
  } else if (additional === 24) {
    need(bytes, cursor, 1);
    argument = BigInt(bytes[cursor]);
    cursor += 1;
  } else if (additional === 25) {
    need(bytes, cursor, 2);
    argument = BigInt((bytes[cursor] << 8) | bytes[cursor + 1]);
    cursor += 2;
  } else if (additional === 26) {
    need(bytes, cursor, 4);
    argument = ZERO;
    for (let index = 0; index < 4; index += 1) {
      argument = (argument << EIGHT) | BigInt(bytes[cursor + index]);
    }
    cursor += 4;
  } else if (additional === 27) {
    need(bytes, cursor, 8);
    argument = ZERO;
    for (let index = 0; index < 8; index += 1) {
      argument = (argument << EIGHT) | BigInt(bytes[cursor + index]);
    }
    cursor += 8;
  } else if (additional === 31) {
    if (major === MAJOR_UNSIGNED || major === MAJOR_NEGATIVE) {
      throw new CborError('indefinite length on an integer');
    }
    if (major === MAJOR_TAG) {
      throw new CborError('indefinite length on a tag');
    }
    indefinite = true;
  } else {
    throw new CborError(`reserved additional information ${additional}`);
  }

  return { major, additional, argument, indefinite, payload: cursor };
};

/**
 * A length taken from a head, as a number. Anything that could not be a length
 * in a buffer this process is holding is a malformed input rather than a very
 * large item.
 */
const lengthOf = (head: CborHead, bytes: Uint8Array): number => {
  if (head.argument > BigInt(bytes.length)) {
    throw new CborError('length exceeds the input');
  }
  return Number(head.argument);
};

/** The offset just past the item that starts at `offset`. */
export const itemEnd = (
  bytes: Uint8Array,
  offset: number,
  depth = 0
): number => {
  if (depth > CBOR_MAX_DEPTH) throw new CborError('nesting too deep');
  const head = readHead(bytes, offset);
  let cursor = head.payload;

  switch (head.major) {
    case MAJOR_UNSIGNED:
    case MAJOR_NEGATIVE:
      return cursor;
    case MAJOR_BYTES:
    case MAJOR_TEXT: {
      if (head.indefinite) {
        for (;;) {
          need(bytes, cursor, 1);
          if (bytes[cursor] === BREAK) return cursor + 1;
          const chunk = readHead(bytes, cursor);
          if (chunk.major !== head.major || chunk.indefinite) {
            throw new CborError('bad chunk in an indefinite string');
          }
          cursor = itemEnd(bytes, cursor, depth + 1);
        }
      }
      const length = lengthOf(head, bytes);
      need(bytes, cursor, length);
      return cursor + length;
    }
    case MAJOR_ARRAY: {
      if (head.indefinite) {
        for (;;) {
          need(bytes, cursor, 1);
          if (bytes[cursor] === BREAK) return cursor + 1;
          cursor = itemEnd(bytes, cursor, depth + 1);
        }
      }
      const count = lengthOf(head, bytes);
      for (let index = 0; index < count; index += 1) {
        cursor = itemEnd(bytes, cursor, depth + 1);
      }
      return cursor;
    }
    case MAJOR_MAP: {
      if (head.indefinite) {
        for (;;) {
          need(bytes, cursor, 1);
          if (bytes[cursor] === BREAK) return cursor + 1;
          cursor = itemEnd(bytes, cursor, depth + 1);
          cursor = itemEnd(bytes, cursor, depth + 1);
        }
      }
      const count = lengthOf(head, bytes);
      for (let index = 0; index < count; index += 1) {
        cursor = itemEnd(bytes, cursor, depth + 1);
        cursor = itemEnd(bytes, cursor, depth + 1);
      }
      return cursor;
    }
    case MAJOR_TAG:
      return itemEnd(bytes, cursor, depth + 1);
    case MAJOR_SIMPLE:
      // Nothing follows the head. A half, single or double float is carried in
      // the head's two, four or eight argument bytes, and `readHead` has
      // already consumed them, as it has the one-byte simple value for
      // additional information 24.
      return cursor;
    default:
      throw new CborError('unreachable major type');
  }
};

/** The span of the item that starts at `offset`. */
export const itemSpan = (bytes: Uint8Array, offset: number): CborSpan => ({
  start: offset,
  end: itemEnd(bytes, offset),
});

/** The spans of an array's elements, in order. */
export const arraySpans = (
  bytes: Uint8Array,
  offset: number
): Array<CborSpan> => {
  const head = readHead(bytes, offset);
  if (head.major !== MAJOR_ARRAY) throw new CborError('not an array');
  const spans: Array<CborSpan> = [];
  let cursor = head.payload;
  if (head.indefinite) {
    for (;;) {
      need(bytes, cursor, 1);
      if (bytes[cursor] === BREAK) break;
      const end = itemEnd(bytes, cursor);
      spans.push({ start: cursor, end });
      cursor = end;
    }
    return spans;
  }
  const count = lengthOf(head, bytes);
  for (let index = 0; index < count; index += 1) {
    const end = itemEnd(bytes, cursor);
    spans.push({ start: cursor, end });
    cursor = end;
  }
  return spans;
};

export type CborEntrySpan = {
  key: CborSpan;
  value: CborSpan;
};

/** The key and value spans of a map's entries, in order. */
export const mapSpans = (
  bytes: Uint8Array,
  offset: number
): Array<CborEntrySpan> => {
  const head = readHead(bytes, offset);
  if (head.major !== MAJOR_MAP) throw new CborError('not a map');
  const entries: Array<CborEntrySpan> = [];
  let cursor = head.payload;
  const readEntry = () => {
    const keyEnd = itemEnd(bytes, cursor);
    const valueEnd = itemEnd(bytes, keyEnd);
    entries.push({
      key: { start: cursor, end: keyEnd },
      value: { start: keyEnd, end: valueEnd },
    });
    cursor = valueEnd;
  };
  if (head.indefinite) {
    for (;;) {
      need(bytes, cursor, 1);
      if (bytes[cursor] === BREAK) break;
      readEntry();
    }
    return entries;
  }
  const count = lengthOf(head, bytes);
  for (let index = 0; index < count; index += 1) readEntry();
  return entries;
};

/**
 * The integer at `offset`, exactly. A mint quantity can exceed what a JavaScript
 * number holds and can be negative, so this returns a bigint rather than
 * rounding one.
 */
export const readInteger = (bytes: Uint8Array, offset: number): bigint => {
  const head = readHead(bytes, offset);
  if (head.major === MAJOR_UNSIGNED) return head.argument;
  if (head.major === MAJOR_NEGATIVE) return -ONE - head.argument;
  throw new CborError('not an integer');
};

/** The byte string at `offset`, with indefinite-length chunks joined. */
export const readBytes = (bytes: Uint8Array, offset: number): Uint8Array => {
  const head = readHead(bytes, offset);
  if (head.major !== MAJOR_BYTES) throw new CborError('not a byte string');
  if (!head.indefinite) {
    const length = lengthOf(head, bytes);
    need(bytes, head.payload, length);
    return bytes.subarray(head.payload, head.payload + length);
  }
  const chunks: Array<Uint8Array> = [];
  let cursor = head.payload;
  let total = 0;
  for (;;) {
    need(bytes, cursor, 1);
    if (bytes[cursor] === BREAK) break;
    const chunk = readBytes(bytes, cursor);
    chunks.push(chunk);
    total += chunk.length;
    cursor = itemEnd(bytes, cursor);
  }
  const joined = new Uint8Array(total);
  let written = 0;
  chunks.forEach((chunk) => {
    joined.set(chunk, written);
    written += chunk.length;
  });
  return joined;
};

/** The text string at `offset`, with indefinite-length chunks joined. */
export const readText = (bytes: Uint8Array, offset: number): string => {
  const head = readHead(bytes, offset);
  if (head.major !== MAJOR_TEXT) throw new CborError('not a text string');
  if (!head.indefinite) {
    const length = lengthOf(head, bytes);
    need(bytes, head.payload, length);
    return Buffer.from(
      bytes.subarray(head.payload, head.payload + length)
    ).toString('utf8');
  }
  let text = '';
  let cursor = head.payload;
  for (;;) {
    need(bytes, cursor, 1);
    if (bytes[cursor] === BREAK) break;
    text += readText(bytes, cursor);
    cursor = itemEnd(bytes, cursor);
  }
  return text;
};

/**
 * The span the tag at `offset` wraps, or the item's own span when it carries no
 * tag. Auxiliary data from Alonzo onwards is a tag 259 map, and the caller needs
 * both the tag number and what is inside it.
 */
export const unwrapTag = (
  bytes: Uint8Array,
  offset: number
): { tag: bigint | null; span: CborSpan } => {
  const head = readHead(bytes, offset);
  if (head.major !== MAJOR_TAG) {
    return { tag: null, span: itemSpan(bytes, offset) };
  }
  return {
    tag: head.argument,
    span: itemSpan(bytes, head.payload),
  };
};
