import isInt from 'validator/lib/isInt';

export const isValidWalletName = (walletName) => walletName.length >= 3;

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

export const isValidAmountInLovelaces = (value: string) => (
  isInt(value, {
    allow_leading_zeroes: false,
    min: 1,
    max: 45000000000000000,
  })
);
