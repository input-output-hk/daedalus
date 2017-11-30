import BigNumber from 'bignumber.js';
import isInt from 'validator/lib/isInt';

export const isValidWalletName = (walletName) => {
  const nameLength = walletName.length;
  return nameLength >= 3 && nameLength <= 40;
};

export const isValidWalletPassword = (walletPassword) => {
  // Validation rules:
  // - should contain at least one digit: (?=.*\d)
  // - should contain at least one lower case: (?=.*[a-z])
  // - should contain at least one upper case: (?=.*[A-Z])
  // - should contain at least 7 characters long: .{7,}
  const passwordRegex = /^(?=.*\d)(?=.*[a-z])(?=.*[A-Z]).{7,}$/;
  return passwordRegex.test(walletPassword);
};

// eslint-disable-next-line max-len
export const isValidRepeatPassword = (walletPassword, repeatPassword) => walletPassword === repeatPassword;

export const isNotEmptyString = (value) => value !== '';

export const isValidAmountInLovelaces = (value: string) => {
  const isNumeric = isInt(value, { allow_leading_zeroes: false });
  if (!isNumeric) return false;
  const numericValue = new BigNumber(value);
  const minValue = new BigNumber(1);
  const maxValue = new BigNumber(45000000000000000);
  const isValid = numericValue.gte(minValue) && numericValue.lte(maxValue);
  return isValid;
};
