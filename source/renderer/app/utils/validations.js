// @flow
import BigNumber from 'bignumber.js';
import isInt from 'validator/lib/isInt';
import { every } from 'lodash';

export const isValidWalletName = (walletName: string) => {
  const nameLength = walletName.length;
  return nameLength >= 3 && nameLength <= 40;
};

// Languages like Japanese do not have the concept of letter case
// so this function checks for such characters (or digits)
const isDigitOrCaselessChar = (char: string) =>
  /\p{Decimal_Number}/u.test(char) ||
  (char === char.toUpperCase() && char === char.toLowerCase());

const isPasswordInCaselessLanguage = (password: string) =>
  every(password.split(''), isDigitOrCaselessChar);

/**
 * Unicode compatible validation rules for spending password.
 * Enforces case sensitive validation for languages that have
 * that concept but allows case-insensitiv validation for langs
 * like Kanji that do not use that concept.
 */
export const isValidSpendingPassword = (password: string): boolean => {
  // Validation rules (uses unicode categories for checks):
  // https://github.com/tc39/proposal-regexp-unicode-property-escapes

  // - should contain at least 7 characters long: .{7,}
  if (password.length < 7) return false;
  // - must not contain white spaces
  if (/\p{White_Space}/u.test(password)) return false;
  // - should contain at least one digit: (?=.*\d)
  if (!/\p{Decimal_Number}/u.test(password)) return false;
  // - should contain at least one lower case: (?=.*[а-я])
  if (!/\p{Lowercase}/u.test(password)) {
    return isPasswordInCaselessLanguage(password);
  }
  // - should contain at least one upper case: (?=.*[А-Я])
  if (!/\p{Uppercase}/u.test(password)) {
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
