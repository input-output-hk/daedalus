import { bech32, Decoded } from 'bech32';

const MAX_BECH32_LENGTH_LIMIT = 1023;

const isOneOf = <T>(target: T, options: T | T[]) =>
  (Array.isArray(options) && options.includes(target)) || target === options;

export const assertIsBech32WithPrefix = (
  target: string,
  prefix: string | string[],
  expectedDecodedLength?: number | number[]
): void => {
  let decoded: Decoded;
  try {
    decoded = bech32.decode(target, MAX_BECH32_LENGTH_LIMIT);
  } catch (error) {
    throw new Error(
      `expected bech32-encoded string with '${prefix}' prefix; ${error}`
    );
  }
  if (!isOneOf(decoded.prefix, prefix)) {
    throw new Error(
      `expected bech32 prefix '${prefix}', got '${decoded.prefix}''`
    );
  }
  if (
    expectedDecodedLength &&
    !isOneOf(decoded.words.length, expectedDecodedLength)
  ) {
    throw new Error(
      `expected decoded length of '${expectedDecodedLength}', got '${decoded.words.length}'`
    );
  }
};
