const { isDev } = global.environment;
export const VOTING_REGISTRATION_MIN_WALLET_FUNDS = 25; // 25 ADA | unit: ADA
export const VOTING_REGISTRATION_FEE_CALCULATION_AMOUNT = 1; // 1 ADA | unit: ADA
export const VOTING_REGISTRATION_PIN_CODE_LENGTH = 4;
export const VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS = isDev ? 2 : 10;
export const VOTING_REGISTRATION_TRANSACTION_POLLING_INTERVAL = 1000; // 1 second | unit: milliseconds
export const VOTING_PHASE_CHECK_INTERVAL = 60000; // 60 seconds | unit: milliseconds
export const VOTING_REWARD = 1040000; // 1,040,000 USD | Unit: dollar
