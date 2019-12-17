// @flow
const { isTest } = global.environment;

// All configuration values for timeouts / intervals should go here
export const REPORT_ISSUE_TIME_TRIGGER = isTest ? 12 : 10 * 60; // 10 minutes / 12 seconds (isTest = true) | unit: seconds
export const NOTIFICATION_DEFAULT_DURATION = 10 * 1000; // 10 seconds / unit: milliseconds
export const ADDRESS_COPY_NOTIFICATION_SMALL_DURATION = 3; // unit: seconds
export const DELETE_WALLET_COUNTDOWN = 10; // unit: seconds
export const FORM_VALIDATION_DEBOUNCE_WAIT = 250; // unit: milliseconds
export const NODE_UPDATE_POLL_INTERVAL = 5000; // unit: milliseconds
export const ALLOWED_TIME_DIFFERENCE = 15 * 1000000; // 15 seconds | unit: microseconds
export const NETWORK_STATUS_POLL_INTERVAL = 2000; // 2 seconds | unit: milliseconds
export const WALLET_UTXO_API_REQUEST_INTERVAL = 5 * 1000; // 5 seconds | unit: milliseconds
export const STAKE_POOL_TOOLTIP_HOVER_WAIT = 700; // 700 milliseconds | unit: milliseconds
export const COPY_STATE_DIRECTORY_PATH_NOTIFICATION_DURATION = 10; // unit: seconds
export const NEWS_POLL_INTERVAL = 30 * 60 * 1000; // 30 minutes | unit: milliseconds
export const NEWS_POLL_INTERVAL_ON_ERROR = 1 * 60 * 1000; // 1 minute | unit: milliseconds
export const NEWS_POLL_INTERVAL_ON_INCIDENT = 10 * 60 * 1000; // 10 minutes | unit: milliseconds
