'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.MAX_DISCREET_MODE_INPUT_FIELD_VALUE = exports.MIN_DISCREET_MODE_INPUT_FIELD_VALUE = exports.ESCAPE_KEY_CODE = exports.ENTER_KEY_CODE = exports.MAX_TOKENS_ON_SUMMARY_PAGE = exports.TX_AGE_POLLING_THRESHOLD = exports.DECIMAL_PLACES_IN_ADA = exports.MAX_INTEGER_PLACES_IN_ADA = exports.LOVELACES_PER_ADA = exports.MAX_ADA_WALLETS_COUNT = exports.MAX_TRANSACTION_CONFIRMATIONS = exports.MAX_TRANSACTIONS_PER_PAGE = exports.MAX_TRANSACTIONS_ON_SUMMARY_PAGE = void 0;
// ADA
exports.MAX_TRANSACTIONS_ON_SUMMARY_PAGE = 5;
exports.MAX_TRANSACTIONS_PER_PAGE = 50;
exports.MAX_TRANSACTION_CONFIRMATIONS = 20; // maximum number of confirmations shown in the UI
exports.MAX_ADA_WALLETS_COUNT = 30; // 50 is an absolute max due to V1 API per_page limitation
exports.LOVELACES_PER_ADA = 1000000;
exports.MAX_INTEGER_PLACES_IN_ADA = 11;
exports.DECIMAL_PLACES_IN_ADA = 6;
exports.TX_AGE_POLLING_THRESHOLD = 15 * 60 * 1000; // 15 minutes | unit: milliseconds
exports.MAX_TOKENS_ON_SUMMARY_PAGE = 5;
// Keyboard events
exports.ENTER_KEY_CODE = 13;
exports.ESCAPE_KEY_CODE = 27;
// Discreet Mode
exports.MIN_DISCREET_MODE_INPUT_FIELD_VALUE = 0;
exports.MAX_DISCREET_MODE_INPUT_FIELD_VALUE = 45000000000;
//# sourceMappingURL=numbersConfig.js.map
