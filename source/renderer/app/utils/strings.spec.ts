import { hexToPrintableAsciiString } from './strings';

const toHex = (bytes: Array<number>) => Buffer.from(bytes).toString('hex');

describe('hexToPrintableAsciiString', () => {
  it('decodes a name whose bytes are all printable ASCII', () => {
    expect(hexToPrintableAsciiString('54657374636f696e')).toBe('Testcoin');
  });

  it('accepts the first printable byte, 0x20', () => {
    expect(hexToPrintableAsciiString(toHex([0x20]))).toBe(' ');
  });

  it('accepts the last printable byte, 0x7e', () => {
    expect(hexToPrintableAsciiString(toHex([0x7e]))).toBe('~');
  });

  it('rejects the byte below the first printable one, 0x1f', () => {
    expect(hexToPrintableAsciiString(toHex([0x1f]))).toBeNull();
  });

  it('rejects the byte above the last printable one, 0x7f', () => {
    expect(hexToPrintableAsciiString(toHex([0x7f]))).toBeNull();
  });

  it('rejects a null byte', () => {
    expect(hexToPrintableAsciiString(toHex([0x00]))).toBeNull();
  });

  it('rejects a name with one non-printable byte among printable ones', () => {
    expect(
      hexToPrintableAsciiString(toHex([0x55, 0x53, 0x44, 0x1f, 0x43]))
    ).toBeNull();
  });

  it('rejects a high byte', () => {
    expect(hexToPrintableAsciiString(toHex([0xff]))).toBeNull();
  });

  it('rejects bytes that are valid UTF-8 but outside ASCII', () => {
    // c3a9 is 'é' and e29885 is '★'. Both decode cleanly as UTF-8 and neither
    // is accepted, which is the conservative choice while nothing renders a
    // non-ASCII name safely.
    expect(hexToPrintableAsciiString('c3a9')).toBeNull();
    expect(hexToPrintableAsciiString('e29885')).toBeNull();
  });

  it('rejects a 32-byte name of random bytes', () => {
    expect(
      hexToPrintableAsciiString(
        '787c09a71b2eacdc2a7644591bd32426ed996387470bc6ec9574167ccf6af8cf'
      )
    ).toBeNull();
  });

  it('rejects the empty name', () => {
    expect(hexToPrintableAsciiString('')).toBeNull();
  });

  it('rejects an absent name', () => {
    expect(hexToPrintableAsciiString(undefined)).toBeNull();
    expect(hexToPrintableAsciiString(null)).toBeNull();
  });

  it('rejects a hex string of odd length', () => {
    // Buffer.from truncates rather than throwing, so 'abc' would otherwise
    // decode to the single byte 0xab.
    expect(hexToPrintableAsciiString('616263616263616')).toBeNull();
  });

  it('rejects a hex string containing a non-hex character', () => {
    // Buffer.from stops at the first non-hex pair, so this would otherwise
    // decode to the four bytes 'USDC'.
    expect(hexToPrintableAsciiString('55534443zz')).toBeNull();
  });
});
