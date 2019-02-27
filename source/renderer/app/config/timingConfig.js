// @flow
const { isTest } = global.environment;

// All configuration values for timeouts / intervals should go here
/* eslint-disable max-len */
export const REPORT_ISSUE_TIME_TRIGGER = 5 * 60; // 5 minutes | unit: seconds
export const ADDRESS_COPY_NOTIFICATION_DURATION = 10; // unit: seconds
export const ADDRESS_COPY_NOTIFICATION_SMALL_DURATION = 3; // unit: seconds
export const DOWNLOAD_LOGS_SUCCESS_DURATION = 10; // unit: seconds
export const DELETE_WALLET_COUNTDOWN = 10; // unit: seconds
export const FORM_VALIDATION_DEBOUNCE_WAIT = 250; // unit: milliseconds
export const NODE_UPDATE_POLL_INTERVAL = 5000; // unit: milliseconds
export const ALLOWED_TIME_DIFFERENCE = 15 * 1000000; // 15 seconds | unit: microseconds
export const MAX_ALLOWED_STALL_DURATION = 2 * 60 * 1000; // 2 minutes | unit: milliseconds
export const NETWORK_STATUS_REQUEST_TIMEOUT = 30 * 1000; // 30 seconds | unit: milliseconds
export const NETWORK_STATUS_POLL_INTERVAL = 2000; // 2 seconds | unit: milliseconds
export const NTP_IGNORE_CHECKS_GRACE_PERIOD = isTest ? 1000 : 35 * 1000; // 35 seconds | unit: milliseconds
export const EPOCH_DATA_UPDATE_INTERVAL = 10 * 1000; // 10 seconds | unit: milliseconds
/* eslint-disable max-len */
