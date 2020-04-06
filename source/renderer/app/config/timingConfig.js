// @flow
const { isTest, isIncentivizedTestnet } = global.environment;

// All configuration values for timeouts / intervals should go here
export const MAX_ALLOWED_STALL_DURATION = isTest
  ? 12 * 1000 // 12 seconds (isTest = true) | unit: milliseconds
  : (isIncentivizedTestnet ? 10 : 7) * 60 * 1000; // 7 minutes / 10 minutes (isIncentivizedTestnet = true) | unit: milliseconds
export const REPORT_ISSUE_TIME_TRIGGER = isTest
  ? 12 // 12 seconds (isTest = true) | unit: seconds
  : (isIncentivizedTestnet ? 10 : 7) * 60; // 7 minutes / 10 minutes (isIncentivizedTestnet = true) | unit: seconds
export const NOTIFICATION_DEFAULT_DURATION = 10 * 1000; // 10 seconds / unit: milliseconds
export const ADDRESS_COPY_NOTIFICATION_SMALL_DURATION = 3; // unit: seconds
export const DELETE_WALLET_COUNTDOWN = 10; // unit: seconds
export const FORM_VALIDATION_DEBOUNCE_WAIT = 250; // unit: milliseconds
export const NODE_UPDATE_POLL_INTERVAL = 5000; // unit: milliseconds
export const ALLOWED_TIME_DIFFERENCE = 15 * 1000000; // 15 seconds | unit: microseconds
export const NETWORK_STATUS_POLL_INTERVAL = 2000; // 2 seconds | unit: milliseconds
export const NETWORK_CLOCK_POLL_INTERVAL = 1000; // 1 second | unit: milliseconds
export const WALLET_UTXO_API_REQUEST_INTERVAL = 5 * 1000; // 5 seconds | unit: milliseconds
export const STAKE_POOL_TOOLTIP_HOVER_WAIT = 700; // 700 milliseconds | unit: milliseconds
export const COPY_STATE_DIRECTORY_PATH_NOTIFICATION_DURATION = 10; // unit: seconds
export const NEWS_POLL_INTERVAL = 30 * 60 * 1000; // 30 minutes | unit: milliseconds
export const NEWS_POLL_INTERVAL_ON_ERROR = 1 * 60 * 1000; // 1 minute | unit: milliseconds
export const NEWS_POLL_INTERVAL_ON_INCIDENT = 10 * 60 * 1000; // 10 minutes | unit: milliseconds
export const FORCED_WALLET_RESYNC_WAIT = isTest ? 1 : 1000; // 1 second / 1 millisecond (isTest = true) | unit milliseconds
