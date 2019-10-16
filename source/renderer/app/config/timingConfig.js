// @flow
const { isTest } = global.environment;

// All configuration values for timeouts / intervals should go here
/* eslint-disable max-len */
export const REPORT_ISSUE_TIME_TRIGGER = isTest ? 12 : 5 * 60; // 5 minutes / 12 seconds (isTest = true) | unit: seconds
export const ADDRESS_COPY_NOTIFICATION_DURATION = 10; // unit: seconds
export const ADDRESS_COPY_NOTIFICATION_SMALL_DURATION = 3; // unit: seconds
export const DOWNLOAD_LOGS_SUCCESS_DURATION = 10; // unit: seconds
export const DELETE_WALLET_COUNTDOWN = 10; // unit: seconds
export const FORM_VALIDATION_DEBOUNCE_WAIT = 250; // unit: milliseconds
export const ALLOWED_TIME_DIFFERENCE = 15 * 1000000; // 15 seconds | unit: microseconds
export const MAX_ALLOWED_STALL_DURATION = isTest ? 15000 : 2 * 60 * 1000; // 2 minutes / 15 seconds (isTest = true) | unit: milliseconds
export const NETWORK_STATUS_REQUEST_TIMEOUT = 30 * 1000; // 30 seconds | unit: milliseconds
export const NETWORK_STATUS_POLL_INTERVAL = 2000; // 2 seconds | unit: milliseconds
export const NTP_IGNORE_CHECKS_GRACE_PERIOD = isTest ? 500 : 35 * 1000; // 35 seconds | unit: milliseconds
export const NTP_RECHECK_TIMEOUT = 1 * 1000; // 1 second | unit: milliseconds;
export const BLOCK_CONSOLIDATION_IPC_REQUEST_INTERVAL = 10 * 1000; // 10 seconds | unit: milliseconds
export const BLOCK_CONSOLIDATION_API_REQUEST_INTERVAL = 30 * 1000; // 30 seconds | unit: milliseconds
export const WALLET_UTXO_API_REQUEST_INTERVAL = 5 * 1000; // 5 seconds | unit: milliseconds
export const STAKE_POOL_TOOLTIP_HOVER_WAIT = 350; // 350 milliseconds | unit: milliseconds
export const COPY_STATE_DIRECTORY_PATH_NOTIFICATION_DURATION = 10; // unit: seconds
/* eslint-disable max-len */
