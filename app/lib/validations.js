export const isValidWalletName = (walletName) => walletName.length >= 3;

export const isNotEmptyString = (value) => value != '';

export const isValidCurrency = (value) => value === 'ada';
