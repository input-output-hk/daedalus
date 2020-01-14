// @flow
const { isDev, isTest, isIncentivizedTestnetSelfNode } = global.environment;

// Epochs constants go here
export const START_TIME_MAINNET = 1506203091;
export const START_TIME_STAGING = 1506450213;
export const START_TIME_TESTNET = 1537941600;
export const START_TIME_DEVELOPMENT = 1541808003;

export const SLOT_DURATION_MAINNET = 20; // unit: seconds
export const SLOT_DURATION_STAGING = 20; // unit: seconds
export const SLOT_DURATION_TESTNET = 20; // unit: seconds
export const SLOT_DURATION_DEVELOPMENT = 7; // unit: seconds
export const SLOT_DURATION_ITN = 2; // unit: seconds

export const SLOTS_TOTAL =
  isDev || isTest || isIncentivizedTestnetSelfNode ? 150 : 43200;

export const EPOCH_LENGTH_BASE_MAINNET = 2160;
export const EPOCH_LENGTH_BASE_STAGING = 2160;
export const EPOCH_LENGTH_BASE_TESTNET = 2160;
export const EPOCH_LENGTH_BASE_DEVELOPMENT = 2;

export const EPOCH_LENGTH_ITN = SLOTS_TOTAL * SLOT_DURATION_ITN; // 1 day / 5 minutes (isDev || isTest || isIncentivizedTestnetSelfNode = true) | unit: seconds

export const EPOCH_COUNTDOWN_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds;
