export type CborSpan = Readonly<{ start: number; end: number }>;

export type CborItem = Readonly<{
  span: CborSpan;
  major: 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7;
  additionalInformation: number;
  value?: bigint;
  tag?: bigint;
  content?: CborSpan;
  items?: readonly CborItem[];
  entries?: readonly Readonly<{ key: CborItem; value: CborItem }>[];
}>;

export class CborParseError extends Error {
  public constructor(message: string) {
    super(message);
    this.name = 'CborParseError';
  }
}

const MAX_BYTES = 65_536;
const MAX_DEPTH = 128;

type State = { items: number };
type Argument = Readonly<{ next: number; value?: bigint; indefinite: boolean }>;

const fail = (message: string): never => {
  throw new CborParseError(message);
};

const readArgument = (
  bytes: Buffer,
  offset: number,
  information: number
): Argument => {
  if (information < 24)
    return { next: offset, value: BigInt(information), indefinite: false };
  const widths: Record<number, number> = { 24: 1, 25: 2, 26: 4, 27: 8 };
  const width = widths[information] || 0;
  if (width) {
    if (offset + width > bytes.length) fail('truncated argument');
    let value = BigInt(0);
    for (let index = 0; index < width; index += 1)
      value = (value << BigInt(8)) | BigInt(bytes[offset + index]);
    return { next: offset + width, value, indefinite: false };
  }
  if (information === 31) return { next: offset, indefinite: true };
  return fail('reserved additional information');
};

const length = (argument: Argument, kind: string): number => {
  if (
    argument.indefinite ||
    argument.value === undefined ||
    argument.value > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    fail(`unsafe ${kind} length`);
  }
  return Number(argument.value);
};

const rawHash = (bytes: Buffer, span: CborSpan): number => {
  let hash = 2166136261;
  for (let index = span.start; index < span.end; index += 1)
    hash = Math.imul(hash ^ bytes[index], 16777619);
  return hash >>> 0;
};

const sameBytes = (bytes: Buffer, left: CborSpan, right: CborSpan): boolean => {
  if (left.end - left.start !== right.end - right.start) return false;
  for (let index = 0; index < left.end - left.start; index += 1) {
    if (bytes[left.start + index] !== bytes[right.start + index]) return false;
  }
  return true;
};

const parse = (
  bytes: Buffer,
  start: number,
  depth: number,
  state: State
): CborItem => {
  if (depth > MAX_DEPTH) fail('maximum nesting exceeded');
  if (start < 0 || start >= bytes.length) fail('truncated item');
  if (++state.items > bytes.length) fail('item budget exceeded');
  const initial = bytes[start];
  if (initial === 0xff) fail('unexpected break');
  const major = (initial >> 5) as CborItem['major'];
  const additionalInformation = initial & 0x1f;
  const argument = readArgument(bytes, start + 1, additionalInformation);
  let cursor = argument.next;
  const base = { span: { start, end: cursor }, major, additionalInformation };

  if (major === 0 || major === 1) {
    if (argument.indefinite || argument.value === undefined)
      fail('indefinite integer');
    return {
      ...base,
      span: { start, end: cursor },
      value: major === 0 ? argument.value : -BigInt(1) - argument.value,
    };
  }
  if (major === 2 || major === 3) {
    if (!argument.indefinite) {
      const size = length(argument, 'string');
      if (cursor + size > bytes.length) fail('truncated string');
      return {
        ...base,
        span: { start, end: cursor + size },
        content: { start: cursor, end: cursor + size },
      };
    }
    const items: CborItem[] = [];
    while (cursor < bytes.length) {
      if (bytes[cursor] === 0xff)
        return { ...base, span: { start, end: cursor + 1 }, items };
      const item = parse(bytes, cursor, depth + 1, state);
      if (item.major !== major || item.content === undefined)
        fail('invalid indefinite string chunk');
      items.push(item);
      cursor = item.span.end;
    }
    return fail('unterminated string');
  }
  if (major === 4) {
    const items: CborItem[] = [];
    const add = () => {
      const item = parse(bytes, cursor, depth + 1, state);
      items.push(item);
      cursor = item.span.end;
    };
    if (argument.indefinite) {
      while (cursor < bytes.length) {
        if (bytes[cursor] === 0xff)
          return { ...base, span: { start, end: cursor + 1 }, items };
        add();
      }
      return fail('unterminated array');
    }
    for (let count = length(argument, 'array'); count > 0; count -= 1) add();
    return { ...base, span: { start, end: cursor }, items };
  }
  if (major === 5) {
    const entries: Array<{ key: CborItem; value: CborItem }> = [];
    const encoded = new Map<number, CborSpan[]>();
    const scalars = new Set<string>();
    const add = () => {
      const key = parse(bytes, cursor, depth + 1, state);
      cursor = key.span.end;
      const value = parse(bytes, cursor, depth + 1, state);
      cursor = value.span.end;
      const hash = rawHash(bytes, key.span);
      const bucket = encoded.get(hash) || [];
      if (bucket.some((other) => sameBytes(bytes, other, key.span)))
        fail('duplicate encoded map key');
      bucket.push(key.span);
      encoded.set(hash, bucket);
      if ((key.major === 0 || key.major === 1) && key.value !== undefined) {
        const scalar = `${key.major}:${key.value.toString()}`;
        if (scalars.has(scalar)) fail('duplicate integer map key');
        scalars.add(scalar);
      }
      entries.push({ key, value });
    };
    if (argument.indefinite) {
      while (cursor < bytes.length) {
        if (bytes[cursor] === 0xff)
          return { ...base, span: { start, end: cursor + 1 }, entries };
        add();
      }
      return fail('unterminated map');
    }
    for (let count = length(argument, 'map'); count > 0; count -= 1) add();
    return { ...base, span: { start, end: cursor }, entries };
  }
  if (major === 6) {
    if (argument.indefinite || argument.value === undefined)
      fail('indefinite tag');
    const item = parse(bytes, cursor, depth + 1, state);
    return {
      ...base,
      span: { start, end: item.span.end },
      tag: argument.value,
      items: [item],
    };
  }
  if (argument.indefinite) fail('unexpected break');
  return { ...base, span: { start, end: cursor } };
};

export const parseCborItem = (bytes: Buffer, start = 0): CborItem => {
  if (!Buffer.isBuffer(bytes) || bytes.length === 0 || bytes.length > MAX_BYTES)
    fail('invalid CBOR input length');
  return parse(bytes, start, 0, { items: 0 });
};

export const bytesForSpan = (bytes: Buffer, span: CborSpan): Buffer =>
  bytes.subarray(span.start, span.end);
