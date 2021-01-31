// @flow
const { isDev } = global.environment;

export const VOTING_REGISTRATION_MIN_WALLET_FUNDS = 7950; // 7950 ADA | unit: ADA
export const VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT = 1; // 1 ADA | unit: ADA
export const VOTING_REGISTRATION_PIN_CODE_LENGTH = 4;
export const VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS = isDev ? 3 : 30;
export const VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL = 1000; // 1 second | unit: milliseconds
export const VOTING_REGISTRATION_CONFIRMATION_DURATION = 60 * 60; // 60 minutes | unit: seconds
