// @flow
const { isDev } = global.environment;

export const VOTING_FUND_NUMBER = 3;
export const VOTING_REGISTRATION_MIN_WALLET_FUNDS = 2950; // 2950 ADA | unit: ADA
export const VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT = 1; // 1 ADA | unit: ADA
export const VOTING_REGISTRATION_PIN_CODE_LENGTH = 4;
export const VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS = isDev ? 2 : 10;
export const VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL = 5000; // 5 seconds | unit: milliseconds
