'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.TOOLTIP_DELAY = exports.TOGGLE_TOKEN_FAVORITE_TIMEOUT = exports.DECENTRALIZATION_LEVEL_POLLING_INTERVAL = exports.ASSET_TOKEN_DISPLAY_DELAY = exports.ASSET_TOKEN_ID_COPY_FEEDBACK = exports.STAKE_POOL_ID_COPY_FEEDBACK = exports.NEWS_POLL_INTERVAL_ON_INCIDENT = exports.NEWS_POLL_INTERVAL_ON_ERROR = exports.NEWS_POLL_INTERVAL = exports.COPY_STATE_DIRECTORY_PATH_NOTIFICATION_DURATION = exports.STAKE_POOL_TOOLTIP_HOVER_WAIT = exports.WALLET_UTXO_API_REQUEST_INTERVAL = exports.NETWORK_CLOCK_POLL_INTERVAL = exports.NETWORK_STATUS_POLL_INTERVAL = exports.ALLOWED_TIME_DIFFERENCE = exports.FORM_VALIDATION_DEBOUNCE_WAIT = exports.DELETE_WALLET_COUNTDOWN = exports.ADDRESS_COPY_NOTIFICATION_SMALL_DURATION = exports.NOTIFICATION_DEFAULT_DURATION = exports.REPORT_ISSUE_TIME_TRIGGER = exports.MAX_ALLOWED_STALL_DURATION = void 0;
const { isTest } = global.environment;
// All configuration values for timeouts / intervals should go here
exports.MAX_ALLOWED_STALL_DURATION = isTest
  ? 12 * 1000 // 12 seconds (isTest = true) | unit: milliseconds
  : 7 * 60 * 1000;
// 7 minutes | unit: milliseconds
exports.REPORT_ISSUE_TIME_TRIGGER = isTest ? 12 : 20 * 60; // 20 minutes | unit: seconds
exports.NOTIFICATION_DEFAULT_DURATION = 10 * 1000; // 10 seconds / unit: milliseconds
exports.ADDRESS_COPY_NOTIFICATION_SMALL_DURATION = 3; // unit: seconds
exports.DELETE_WALLET_COUNTDOWN = 10; // unit: seconds
exports.FORM_VALIDATION_DEBOUNCE_WAIT = 250; // unit: milliseconds
exports.ALLOWED_TIME_DIFFERENCE = 4.5 * 1000000; // 4.5 seconds | unit: microseconds
exports.NETWORK_STATUS_POLL_INTERVAL = 2000; // 2 seconds | unit: milliseconds
exports.NETWORK_CLOCK_POLL_INTERVAL = 1000; // 1 second | unit: milliseconds
exports.WALLET_UTXO_API_REQUEST_INTERVAL = 5 * 1000; // 5 seconds | unit: milliseconds
exports.STAKE_POOL_TOOLTIP_HOVER_WAIT = 500; // 500 milliseconds | unit: milliseconds
exports.COPY_STATE_DIRECTORY_PATH_NOTIFICATION_DURATION = 10; // unit: seconds
exports.NEWS_POLL_INTERVAL = 30 * 60 * 1000; // 30 minutes | unit: milliseconds
exports.NEWS_POLL_INTERVAL_ON_ERROR = 1 * 60 * 1000; // 1 minute | unit: milliseconds
exports.NEWS_POLL_INTERVAL_ON_INCIDENT = 10 * 60 * 1000; // 10 minutes | unit: milliseconds
exports.STAKE_POOL_ID_COPY_FEEDBACK = 3000; // 1.5 second | unit: milliseconds
exports.ASSET_TOKEN_ID_COPY_FEEDBACK = 3 * 1000; // 3 seconds | unit: milliseconds
exports.ASSET_TOKEN_DISPLAY_DELAY = 250; // .25 second | unit: milliseconds
exports.DECENTRALIZATION_LEVEL_POLLING_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds
exports.TOGGLE_TOKEN_FAVORITE_TIMEOUT = 300; // .3 second | unit: milliseconds
exports.TOOLTIP_DELAY = [300, 0]; // [enter, leave] .3 second | unit: milliseconds
//# sourceMappingURL=timingConfig.js.map
