// @flow
import BigNumber from 'bignumber.js';
import isInt from 'validator/lib/isInt';

export const isValidWalletName = (walletName: string) => {
  const nameLength = walletName.length;
  return nameLength >= 3 && nameLength <= 40;
};

export const isValidSpendingPassword = (spendingPassword: string) => {
  // Validation rules:
  // - should contain at least one digit: (?=.*\d)
  // - should contain at least one lower case: (?=.*[а-я])
  // - should contain at least one upper case: (?=.*[А-Я])
  // - should contain at least 7 characters long: .{7,}
  const passwordRegex = /^(?=.*\d)(?=.*[а-я])(?=.*[А-Я]).{7,}$/;
  return passwordRegex.test(spendingPassword);
};

// eslint-disable-next-line max-len
export const isValidRepeatPassword = (spendingPassword: string, repeatPassword: string) => (
  spendingPassword === repeatPassword
);

export const isNotEmptyString = (value: string) => value !== '';

export const isValidAmountInLovelaces = (value: string) => {
  const isNumeric = isInt(value, { allow_leading_zeroes: false });
  if (!isNumeric) return false;
  const numericValue = new BigNumber(value);
  const minValue = new BigNumber(1);
  const maxValue = new BigNumber(45000000000000000);
  return numericValue.gte(minValue) && numericValue.lte(maxValue);
};
