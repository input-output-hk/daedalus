// @flow
const { isShelleyTestnet, isIncentivizedTestnet, environment } = global;
const { isDev, isTest, isIncentivizedTestnetSelfnode } = environment;

// Epochs constants go here
export const START_TIME_MAINNET = 1506203091;
export const START_TIME_STAGING = 1506450213;
export const START_TIME_TESTNET = 1537941600;
export const START_TIME_DEVELOPMENT = 1541808003;

export const SLOT_DURATION = 1; // unit: seconds
export const SLOT_DURATION_STN = 1; // unit: seconds
export const SLOT_DURATION_ITN = 2; // unit: seconds
export const SLOT_DURATION_SHELLEY = isDev ? 7 : 20;

export const TOTAL_SLOTS = 432000;
export const TOTAL_SLOTS_STN = 21600;
export const TOTAL_SLOTS_ITN =
  isDev || isTest || isIncentivizedTestnetSelfnode ? 150 : 43200;
// eslint-disable-next-line
export const TOTAL_SLOTS_SHELLEY = isDev
  ? 1541808003
  : isTest
  ? 1537941600
  : 1506203091;

export const EPOCH_LENGTH = SLOT_DURATION * TOTAL_SLOTS; // 5 days | unit: seconds
export const EPOCH_LENGTH_STN = SLOT_DURATION_STN * TOTAL_SLOTS_STN; // 6 hours | unit: seconds
export const EPOCH_LENGTH_ITN = SLOT_DURATION_ITN * TOTAL_SLOTS_ITN; // 1 day / 5 minutes (isDev || isTest || isIncentivizedTestnetSelfnode = true) | unit: seconds
export const EPOCH_LENGTH_SHELLEY = SLOT_DURATION_SHELLEY * TOTAL_SLOTS_SHELLEY;

export const EPOCH_COUNTDOWN_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds;

export const getTotalSlots = () => {
  if (isIncentivizedTestnet) return TOTAL_SLOTS_ITN;
  if (isShelleyTestnet) return TOTAL_SLOTS_STN;
  return TOTAL_SLOTS;
};

export const getEpochLength = () => {
  if (isIncentivizedTestnet) return EPOCH_LENGTH_ITN;
  if (isShelleyTestnet) return EPOCH_LENGTH_STN;
  return EPOCH_LENGTH;
};
