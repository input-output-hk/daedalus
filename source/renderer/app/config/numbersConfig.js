import BigNumber from 'bignumber.js';

// ADA
export const MAX_ADA_WALLETS_COUNT = 20; // 50 is an absolute max due to V1 API per_page limitation
export const LOVELACES_PER_ADA = 1000000;
export const MAX_INTEGER_PLACES_IN_ADA = 11;
export const DECIMAL_PLACES_IN_ADA = 6;

// ETC
export const WEI_PER_ETC = 1000000000000000000;
export const DECIMAL_PLACES_IN_ETC = 18;
export const ETC_DEFAULT_GAS_PRICE = new BigNumber(10).pow(10).times(2);
export const HARDCODED_ETC_TX_FEE = 0.00042;
