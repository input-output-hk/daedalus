// @flow

// ADA
export const MAX_TRANSACTIONS_ON_SUMMARY_PAGE = 5;
export const MAX_TRANSACTIONS_PER_PAGE = 50;
export const MAX_TRANSACTION_CONFIRMATIONS = 20; // maximum number of confirmations shown in the UI
export const MAX_ADA_WALLETS_COUNT = 20; // 50 is an absolute max due to V1 API per_page limitation
export const LOVELACES_PER_ADA = 1000000;
export const MAX_INTEGER_PLACES_IN_ADA = 11;
export const DECIMAL_PLACES_IN_ADA = 6;
export const SIMPLE_DECIMAL_PLACES_IN_ADA = 2;
export const TX_UNCONFIRMED_THRESHOLD = 6;
export const TX_AGE_POLLING_THRESHOLD = 15 * 60 * 1000; // 15 minutes | unit: milliseconds
