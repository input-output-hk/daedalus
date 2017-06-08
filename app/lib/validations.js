import isInt from 'validator/lib/isInt';

export const isValidWalletName = (walletName) => walletName.length >= 3;

export const isValidWalletPassword = (walletPassword) => walletPassword.length >= 6;

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
