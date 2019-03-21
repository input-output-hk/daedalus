// @flow
import BigNumber from 'bignumber.js';
import isInt from 'validator/lib/isInt';
import { every } from 'lodash';

export const isValidWalletName = (walletName: string) => {
  const nameLength = walletName.length;
  return nameLength >= 3 && nameLength <= 40;
};

/**
 * Checks if a character is either a caseless letter or a digit
 * (Languages like Japanese do not have the concept of letter case)
 */
const isDigitOrCaselessChar = (char: string) =>
  /^\p{Decimal_Number}$/u.test(char) ||
  (char === char.toUpperCase() && char === char.toLowerCase());

/**
 * Test if a whole password is in caseless letters (or digits)
 */
const isPasswordInCaselessLanguage = (password: string) =>
  every(password.split(''), isDigitOrCaselessChar);

/**
 * Unicode compatible validation rules for spending password.
 * Enforces case sensitive validation for languages that have that concept
 * but allows case-insensitiv validation for langs like Kanji
 */
export const isValidSpendingPassword = (password: string): boolean => {
  // Validation rules (uses unicode categories for checks):
  // https://github.com/tc39/proposal-regexp-unicode-property-escapes

  // Should contain at least 7 characters
  if (password.length < 7) return false;

  // Must not contain white spaces
  if (/\p{White_Space}/u.test(password)) return false;

  // Should contain at least one digit
  if (!/\p{Decimal_Number}/u.test(password)) return false;

  // Should contain at least one lower case
  if (!/\p{Lowercase}/u.test(password)) {
    // Should still allow passwords in caseless languages like Kanji
    return isPasswordInCaselessLanguage(password);
  }

  // Should contain at least one upper case
  if (!/\p{Uppercase}/u.test(password)) {
    // Should still allow passwords in caseless languages like Kanji
    return isPasswordInCaselessLanguage(password);
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
