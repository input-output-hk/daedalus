import isInt from 'validator/lib/isInt';

export const isValidWalletName = (walletName) => walletName.length >= 3;

export const isNotEmptyString = (value) => value !== '';

export const isValidCurrency = (value) => value === 'ada';

export const isValidAmountInLovlelaces = (value: string) => (
  isInt(value, {
    allow_leading_zeroes: false,
    min: 1,
    max: 45000000000000000,
  })
);
