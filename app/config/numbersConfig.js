import BigNumber from 'bignumber.js';

export const LOVELACES_PER_ADA = 1000000;
export const WEI_PER_ETC = 1000000000000000000;
export const MAX_INTEGER_PLACES_IN_ADA = 11;
export const DECIMAL_PLACES_IN_ADA = 6;
export const DECIMAL_PLACES_IN_ETC = 18;
export const ETC_DEFAULT_GAS_PRICE = new BigNumber(10).pow(10).times(2);
