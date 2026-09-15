export const ellipsis = (
  str: string,
  minCharsInit: number,
  minCharsEnd?: number | null | undefined
) => {
  if (str.length <= minCharsInit) return str;
  const initStr = str.substr(0, minCharsInit);

  const shouldHaveEndStr = () =>
    minCharsEnd && str.length - minCharsInit > minCharsEnd;

  const endStr =
    minCharsEnd && shouldHaveEndStr()
      ? str.substr(str.length - minCharsEnd)
      : '';
  return `${initStr}\u2026${endStr}`;
};
export const hexToString = (valueInHex: string): string =>
  Buffer.from(valueInHex, 'hex').toString();

const PRINTABLE_ASCII_FIRST_BYTE = 0x20;
const PRINTABLE_ASCII_LAST_BYTE = 0x7e;
const HEX_BYTES = /^(?:[0-9a-fA-F]{2})+$/;

/**
 * Decodes a hex-encoded asset name and returns it only when it is text.
 *
 * An asset name is a free-form byte string chosen by whoever minted the asset,
 * so most of them are not text at all. This returns the decoded name only when
 * every decoded byte is printable ASCII, `0x20` to `0x7E` inclusive, and `null`
 * otherwise: for the empty name, for a malformed hex string, and for any byte
 * outside that range.
 *
 * The hex string is validated as well as the bytes. `Buffer.from` stops at the
 * first non-hex pair and truncates an odd-length input rather than throwing, so
 * `Buffer.from('55534443zz', 'hex')` yields the four bytes `USDC`. Accepting a
 * silently truncated prefix would defeat the point of the check.
 */
export const hexToPrintableAsciiString = (
  valueInHex?: string | null
): string | null => {
  if (!valueInHex || !HEX_BYTES.test(valueInHex)) return null;
  const bytes = Buffer.from(valueInHex, 'hex');
  const isPrintable = bytes.every(
    (byte) =>
      byte >= PRINTABLE_ASCII_FIRST_BYTE && byte <= PRINTABLE_ASCII_LAST_BYTE
  );
  // `latin1` maps each byte to the character of the same code point. Over the
  // accepted range it agrees with `ascii`, which masks the high bit and so
  // could turn a rejected byte into an accepted character.
  return isPrintable ? bytes.toString('latin1') : null;
};
