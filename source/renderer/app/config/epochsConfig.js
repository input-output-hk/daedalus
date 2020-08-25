// @flow
const { isShelleyTestnet, isIncentivizedTestnet, environment } = global;
const { isDev, isTest, isIncentivizedTestnetSelfnode } = environment;

// Epochs constants go here
export const START_TIME_MAINNET = 1506203091;
export const START_TIME_STAGING = 1506450213;
export const START_TIME_TESTNET = 1537941600;
export const START_TIME_DEVELOPMENT = 1541808003;

export const DEFAULT_SLOT_LENGTH = { quantity: 1, unit: 'second' };
export const DEFAULT_SLOT_LENGTH_STN = { quantity: 1, unit: 'second' };
export const DEFAULT_SLOT_LENGTH_ITN = { quantity: 2, unit: 'second' };

export const DEFAULT_EPOCH_LENGTH = { quantity: 432000, unit: 'slot' };
export const DEFAULT_EPOCH_LENGTH_STN = { quantity: 21600, unit: 'slot' };
export const DEFAULT_EPOCH_LENGTH_ITN =
  isDev || isTest || isIncentivizedTestnetSelfnode
    ? { quantity: 150, unit: 'slot' }
    : { quantity: 43200, unit: 'slot' };

export const DEFAULT_EPOCH_COUNTDOWN_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds;

export const getDefaultSlotLength = () => {
  if (isIncentivizedTestnet) return DEFAULT_SLOT_LENGTH_ITN;
  if (isShelleyTestnet) return DEFAULT_SLOT_LENGTH_STN;
  return DEFAULT_SLOT_LENGTH;
};

export const getDefaultEpochLength = () => {
  if (isIncentivizedTestnet) return DEFAULT_EPOCH_LENGTH_ITN;
  if (isShelleyTestnet) return DEFAULT_EPOCH_LENGTH_STN;
  return DEFAULT_EPOCH_LENGTH;
};
