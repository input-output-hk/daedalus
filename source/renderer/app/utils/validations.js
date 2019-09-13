// @flow
import BigNumber from 'bignumber.js';
import isInt from 'validator/lib/isInt';
import { every } from 'lodash';

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
 * Checks if a string doesn't change after uppper and lower casing it
 */
export const isCaselessString = (s: string) =>
  s.toLowerCase() === s && s.toUpperCase() === s;

/**
 * Test if a whole string is in unicase letters (or digits)
 */
export const isUnicaseString = (password: string) =>
  // We require at least one unicase letter
  containsUnicaseLetter(password) &&
  // Every char has to belong to the support caseless categories
  every(password.split(''), char => isCaselessString(char));

/**
 * Unicode compatible validation rules for spending password.
 * Enforces case sensitive validation for languages that have that concept
 * but allows case-insensitiv validation for langs like Kanji
 */
export const isValidSpendingPassword = (password: string): boolean => {
  // Validation rules (uses unicode categories for checks):
  // https://github.com/tc39/proposal-regexp-unicode-property-escapes

  // Should contain at least 10 characters
  if (password.length < 10) return false;

  // Must not contain white spaces
  if (containsWhitespace(password)) return false;

  // Should contain at least one digit
  if (!containsDecimalNumber(password)) return false;

  // Should contain at least one lower case
  if (!containsLowerCaseLetter(password)) {
    // But allow passwords in unicase languages like Kanji
    return isUnicaseString(password);
  }
  // Should contain at least one upper case
  if (!containsUpperCaseLetter(password)) {
    // But allow passwords in unicase languages like Kanji
    return isUnicaseString(password);
  }
  return true;
};

// eslint-disable-next-line max-len
export const isValidRepeatPassword = (
  spendingPassword: string,
  repeatPassword: string
) => spendingPassword === repeatPassword;

export const isNotEmptyString = (value: string) => value !== '';

export const isValidAmountInLovelaces = (value: string) => {
  const isNumeric = isInt(value, { allow_leading_zeroes: false });
  if (!isNumeric) return false;
  const numericValue = new BigNumber(value);
  const minValue = new BigNumber(1);
  const maxValue = new BigNumber(45000000000000000);
  return numericValue.gte(minValue) && numericValue.lte(maxValue);
};
