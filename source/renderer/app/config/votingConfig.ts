const { isDev } = global.environment;
export const CURRENT_VOTING_FUND_NUMBER = 7;
export const NEXT_VOTING_FUND_NUMBER = CURRENT_VOTING_FUND_NUMBER + 1;
export const VOTING_REGISTRATION_MIN_WALLET_FUNDS = 500; // 500 ADA | unit: ADA

export const VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT = 1; // 1 ADA | unit: ADA

export const VOTING_REGISTRATION_PIN_CODE_LENGTH = 4;
export const VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS = isDev ? 2 : 10;
export const VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL = 1000; // 1 second | unit: milliseconds

export const VOTING_PHASE_CHECK_INTERVAL = 60000; // 60 seconds | unit: milliseconds

export const VOTING_SNAPSHOT_DATE = new Date('Jan 6, 2022, 11:00 UTC');
export const VOTING_CAST_START_DATE = new Date('Jan 13, 2022, 11:00 UTC');
export const VOTING_CAST_END_DATE = new Date('Jan 27, 2022, 11:00 UTC');
export const VOTING_RESULTS_DATE = new Date('Feb 3, 2022');
export const VOTING_NEW_SNAPSHOT_DATE = new Date('Apr 7, 2022, 11:00 UTC');
export const VOTING_REWARD = 1040000; // 1,040,000 USD | Unit: dollar
