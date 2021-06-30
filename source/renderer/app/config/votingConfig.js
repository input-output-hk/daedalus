// @flow
const { isDev } = global.environment;

export const IS_VOTING_REGISTRATION_AVAILABLE = false;
export const VOTING_FUND_NUMBER = 5;
export const VOTING_REGISTRATION_MIN_WALLET_FUNDS = 500; // 500 ADA | unit: ADA
export const VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT = 1; // 1 ADA | unit: ADA
export const VOTING_REGISTRATION_PIN_CODE_LENGTH = 4;
export const VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS = isDev ? 2 : 10;
export const VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL = 1000; // 1 second | unit: milliseconds
export const VOTING_REGISTRATION_END_DATE = new Date('Jul 19, 2021, 11:00 UTC');
export const VOTING_REGISTRATION_END_CHECK_INTERVAL = 3000; // 3 seconds | unit: milliseconds
export const VOTING_REGISTRATION_CAST_START_DATE = new Date(
  'Jul 22, 2021, 11:00 UTC'
);
export const VOTING_REGISTRATION_CAST_END_DATE = new Date(
  'Aug 2, 2021, 11:00 UTC'
);
export const VOTING_REGISTRATION_NEW_START_DATE = new Date(
  'Aug 12, 2021, 11:00 UTC'
);
