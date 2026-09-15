/**
 * The structural reader, driven against bytes produced by an encoder nobody
 * here wrote.
 *
 * `cbor@5.0.2` is used to build the inputs. That is the point of the pairing:
 * the module under test never encodes anything, so an agreement between the two
 * is evidence about the reader rather than about a shared assumption. The
 * hand-written inputs are the ones `cbor.encode` will not produce, which is
 * where a reader is most likely to be wrong.
 */
import * as cbor from 'cbor';
import {
  CBOR_MAX_DEPTH,
  CborError,
  arraySpans,
  itemEnd,
  itemSpan,
  mapSpans,
  readHead,
  readInteger,
  readBytes,
  readText,
  unwrapTag,
} from './cborSpan';

const bytes = (...values: Array<number>): Uint8Array => Uint8Array.from(values);

const encoded = (value: unknown): Uint8Array =>
  Uint8Array.from(cbor.encode(value));

describe('itemEnd', () => {
  it('measures every argument width an integer can carry', () => {
    [0, 23, 24, 255, 256, 65535, 65536, 4294967295, 4294967296].forEach(
      (value) => {
        const input = encoded(value);
        expect(itemEnd(input, 0)).toBe(input.length);
      }
    );
  });

  it('measures a definite-length byte string and text string', () => {
    const byteString = encoded(Buffer.from('0badc0de', 'hex'));
    expect(itemEnd(byteString, 0)).toBe(byteString.length);
    const text = encoded('a name with spaces');
    expect(itemEnd(text, 0)).toBe(text.length);
  });

  it('measures a nested array and map', () => {
    const input = encoded([1, [2, 3], new Map([[4, [5, 6]]])]);
    expect(itemEnd(input, 0)).toBe(input.length);
  });

  it('measures an indefinite-length array', () => {
    // 9f 01 02 ff
    const input = bytes(0x9f, 0x01, 0x02, 0xff);
    expect(itemEnd(input, 0)).toBe(4);
  });

  it('measures an indefinite-length map', () => {
    // bf 01 02 ff
    const input = bytes(0xbf, 0x01, 0x02, 0xff);
    expect(itemEnd(input, 0)).toBe(4);
  });

  it('measures an indefinite-length byte string', () => {
    // 5f 42 0102 41 03 ff
    const input = bytes(0x5f, 0x42, 0x01, 0x02, 0x41, 0x03, 0xff);
    expect(itemEnd(input, 0)).toBe(7);
  });

  it('measures a tagged item as the tag plus its content', () => {
    const input = Uint8Array.from(cbor.encode(new cbor.Tagged(259, [1, 2])));
    expect(itemEnd(input, 0)).toBe(input.length);
  });

  it('measures the simple values and the float widths', () => {
    expect(itemEnd(bytes(0xf4), 0)).toBe(1); // false
    expect(itemEnd(bytes(0xf5), 0)).toBe(1); // true
    expect(itemEnd(bytes(0xf6), 0)).toBe(1); // null
    expect(itemEnd(bytes(0xf9, 0x00, 0x00), 0)).toBe(3); // half float
    expect(itemEnd(bytes(0xfa, 0, 0, 0, 0), 0)).toBe(5); // single float
    expect(itemEnd(bytes(0xfb, 0, 0, 0, 0, 0, 0, 0, 0), 0)).toBe(9); // double
  });

  it('refuses a truncated item rather than reading past the end', () => {
    expect(() => itemEnd(bytes(0x43, 0x01), 0)).toThrow(CborError);
    expect(() => itemEnd(bytes(0x82, 0x01), 0)).toThrow(CborError);
    expect(() => itemEnd(bytes(0x18), 0)).toThrow(CborError);
  });

  // An indefinite-length string may only hold chunks of its own type, so a
  // chunk of another type is a malformed input rather than a nested item.
  it('refuses a chunk of the wrong type inside an indefinite string', () => {
    expect(() => itemEnd(bytes(0x5f, 0x01, 0xff), 0)).toThrow('bad chunk');
    expect(() => itemEnd(bytes(0x7f, 0x41, 0x61, 0xff), 0)).toThrow(
      'bad chunk'
    );
  });

  it('refuses a reserved additional information value', () => {
    expect(() => itemEnd(bytes(0x1c), 0)).toThrow(CborError);
  });

  it('refuses an indefinite length where the format does not allow one', () => {
    expect(() => itemEnd(bytes(0x1f), 0)).toThrow(CborError);
    expect(() => itemEnd(bytes(0xdf), 0)).toThrow(CborError);
  });

  it('refuses a length that could not fit in the input', () => {
    // A byte string claiming four gigabytes, in six bytes of input.
    expect(() => itemEnd(bytes(0x5a, 0xff, 0xff, 0xff, 0xff), 0)).toThrow(
      CborError
    );
  });

  it('refuses nesting past the depth cap rather than overflowing the stack', () => {
    const deep = new Uint8Array(CBOR_MAX_DEPTH + 10).fill(0x81);
    expect(() => itemEnd(deep, 0)).toThrow('nesting too deep');
  });
});

describe('arraySpans', () => {
  it('returns one span per element, in order', () => {
    const input = encoded([1, 'two', Buffer.from('03', 'hex')]);
    const spans = arraySpans(input, 0);
    expect(spans).toHaveLength(3);
    expect(readInteger(input, spans[0].start)).toBe(BigInt(1));
    expect(readText(input, spans[1].start)).toBe('two');
    expect(Buffer.from(readBytes(input, spans[2].start)).toString('hex')).toBe(
      '03'
    );
  });

  it('returns the spans of an indefinite-length array', () => {
    const input = bytes(0x9f, 0x01, 0x02, 0xff);
    expect(arraySpans(input, 0)).toEqual([
      { start: 1, end: 2 },
      { start: 2, end: 3 },
    ]);
  });

  it('refuses an item that is not an array', () => {
    expect(() => arraySpans(encoded(1), 0)).toThrow('not an array');
  });
});

describe('mapSpans', () => {
  it('returns the key and value spans of every entry', () => {
    const input = encoded(
      new Map<number, unknown>([
        [0, 'zero'],
        [9, 'nine'],
      ])
    );
    const entries = mapSpans(input, 0);
    expect(entries).toHaveLength(2);
    expect(readInteger(input, entries[1].key.start)).toBe(BigInt(9));
    expect(readText(input, entries[1].value.start)).toBe('nine');
  });

  it('returns the entries of an indefinite-length map', () => {
    const input = bytes(0xbf, 0x01, 0x02, 0xff);
    expect(mapSpans(input, 0)).toEqual([
      { key: { start: 1, end: 2 }, value: { start: 2, end: 3 } },
    ]);
  });

  it('refuses an item that is not a map', () => {
    expect(() => mapSpans(encoded([1]), 0)).toThrow('not a map');
  });
});

describe('readInteger', () => {
  it('reads a value larger than a JavaScript number holds exactly', () => {
    // A mint quantity can be up to 2^63 - 1, which loses precision as a number.
    const input = bytes(0x1b, 0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff);
    expect(readInteger(input, 0)).toBe(BigInt('9223372036854775807'));
  });

  it('reads a negative value', () => {
    expect(readInteger(encoded(-1), 0)).toBe(BigInt(-1));
    expect(readInteger(encoded(-1000), 0)).toBe(BigInt(-1000));
  });

  it('refuses an item that is not an integer', () => {
    expect(() => readInteger(encoded('one'), 0)).toThrow('not an integer');
  });
});

describe('readBytes', () => {
  it('joins the chunks of an indefinite-length byte string', () => {
    const input = bytes(0x5f, 0x42, 0x01, 0x02, 0x41, 0x03, 0xff);
    expect(Buffer.from(readBytes(input, 0)).toString('hex')).toBe('010203');
  });

  it('refuses an item that is not a byte string', () => {
    expect(() => readBytes(encoded('text'), 0)).toThrow('not a byte string');
  });
});

describe('readText', () => {
  it('joins the chunks of an indefinite-length text string', () => {
    // 7f 62 6162 61 63 ff
    const input = bytes(0x7f, 0x62, 0x61, 0x62, 0x61, 0x63, 0xff);
    expect(readText(input, 0)).toBe('abc');
  });

  it('refuses an item that is not a text string', () => {
    expect(() => readText(encoded(1), 0)).toThrow('not a text string');
  });
});

describe('unwrapTag', () => {
  it('returns the tag number and the span it wraps', () => {
    const input = Uint8Array.from(cbor.encode(new cbor.Tagged(259, [1])));
    const { tag, span } = unwrapTag(input, 0);
    expect(tag).toBe(BigInt(259));
    expect(arraySpans(input, span.start)).toHaveLength(1);
  });

  it('returns no tag and the item itself for an untagged item', () => {
    const input = encoded([1]);
    const { tag, span } = unwrapTag(input, 0);
    expect(tag).toBeNull();
    expect(span).toEqual(itemSpan(input, 0));
  });
});

describe('readHead', () => {
  it('reads the two-byte argument width', () => {
    const head = readHead(bytes(0x19, 0x01, 0x00), 0);
    expect(head.major).toBe(0);
    expect(head.argument).toBe(BigInt(256));
    expect(head.payload).toBe(3);
  });

  it('reads the eight-byte argument width without losing precision', () => {
    const head = readHead(
      bytes(0x1b, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff),
      0
    );
    expect(head.argument).toBe(BigInt('18446744073709551615'));
  });
});
