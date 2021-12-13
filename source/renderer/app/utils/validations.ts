import BigNumber from 'bignumber.js';
import isInt from 'validator/lib/isInt';
import { every } from 'lodash';

const MIN_PASSWORD_LENGTH = 10;
const MAX_PASSWORD_LENGTH = 255;
export const isValidWalletName = (walletName: string) => {
  const nameLength = walletName.length;
  return nameLength >= 3 && nameLength <= 40;
};

/**
 * Checks if a string contains unicode white space
 */
export const containsWhitespace = (s: string) => /\p{White_Space}/u.test(s);

/**
 * Checks if a string contains a decimal number
 */
export const containsDecimalNumber = (s: string) =>
  /\p{Decimal_Number}/u.test(s);

/**
 * Checks if a string contains a lower case letter
 */
export const containsLowerCaseLetter = (s: string) =>
  /\p{Lowercase_Letter}/u.test(s);

/**
 * Checks if a string contains a upper case letter
 */
export const containsUpperCaseLetter = (s: string) =>
  /\p{Uppercase_Letter}/u.test(s);

/**
 * Checks if a string contains a unicase letter
 * (E.g: Languages like Kanji do not have the concept of letter case)
 */
export const containsUnicaseLetter = (s: string) => /\p{Other_Letter}/u.test(s);

/**
 * Checks if a string doesn't change after upper and lower casing it
 */
export const isCaselessString = (s: string) =>
  s.toLowerCase() === s && s.toUpperCase() === s;

/**
 * Test if a whole string is in unicase letters (or digits)
 */
export const isUnicaseString = (
  password: string // We require at least one unicase letter
) =>
  containsUnicaseLetter(password) && // Every char has to belong to the support caseless categories
  every(password.split(''), (char) => isCaselessString(char));

/**
 * Enforces passwords without spaces and a minimum of 10 characters and a maximum of 255 characters.
 */
export const isValidSpendingPassword = (password: string): boolean => {
  // Should contain at least 10 characters
  return (
    password.length >= MIN_PASSWORD_LENGTH &&
    password.length <= MAX_PASSWORD_LENGTH
  );
};
// eslint-disable-next-line max-len
export const isValidRepeatPassword = (
  spendingPassword: string,
  repeatPassword: string
) => spendingPassword === repeatPassword;
export const isNotEmptyString = (value: string) => value !== '';
export const isValidAmountInLovelaces = (value: string) => {
  const isNumeric = isInt(value, {
    allow_leading_zeroes: false,
  });
  if (!isNumeric) return false;
  const numericValue = new BigNumber(value);
  const minValue = new BigNumber(1);
  const maxValue = new BigNumber(45000000000000000);
  return numericValue.gte(minValue) && numericValue.lte(maxValue);
};
export const isValidAssetAmountInNaturalUnits = (value: string) => {
  const isNumeric = isInt(value, {
    allow_leading_zeroes: false,
  });
  if (!isNumeric) return false;
  const numericValue = new BigNumber(value);
  const minValue = new BigNumber(1);
  const maxValue = new BigNumber('18446744073709551615'); // cardano-wallet max asset amount of 2^64 - 1

  return numericValue.gte(minValue) && numericValue.lte(maxValue);
};

/**
 * Mnemonics validation
 */
type ValidateMnemonicsParams = {
  requiredWords: number | number[];
  providedWords: string[];
  validator: (providedWords: string[]) => [boolean, string];
};
export const INCOMPLETE_MNEMONIC_MARKER = 'INCOMPLETE_MNEMONIC_MARKER';
export function validateMnemonics(params: ValidateMnemonicsParams) {
  const { requiredWords, providedWords } = params;
  const providedWordsCount = providedWords.length;
  const isPhraseComplete = Array.isArray(requiredWords)
    ? requiredWords.includes(providedWordsCount)
    : providedWordsCount === requiredWords;

  if (!isPhraseComplete) {
    return INCOMPLETE_MNEMONIC_MARKER;
  }

  return params.validator(providedWords);
}
export function errorOrIncompleteMarker(error: string) {
  return error === INCOMPLETE_MNEMONIC_MARKER ? null : error;
}

/**
 * Voting PIN code validation
 */
export const isValidPinCode = (pinCode: string, length: number): boolean => {
  return pinCode.length === length;
};
export const isValidRepeatPinCode = (pinCode: string, repeatPinCode: string) =>
  pinCode === repeatPinCode;
