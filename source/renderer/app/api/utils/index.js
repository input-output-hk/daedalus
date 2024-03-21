'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.isLegacyWalletId = exports.getRawWalletId = exports.getLegacyWalletId = exports.getContentLength = exports.encryptPassphrase = exports.utcStringToDate = exports.unixTimestampToDate = void 0;
const moment_1 = __importDefault(require('moment'));
const blakejs_1 = __importDefault(require('blakejs'));
// time utils
const unixTimestampToDate = (timestamp) => new Date(timestamp * 1000);
exports.unixTimestampToDate = unixTimestampToDate;
const utcStringToDate = (createDate) =>
  moment_1.default.utc(createDate).toDate();
exports.utcStringToDate = utcStringToDate;
// passphrase utils
const bytesToB16 = (bytes) => Buffer.from(bytes).toString('hex');
const blake2b = (data) => blakejs_1.default.blake2b(data, null, 32);
const encryptPassphrase = (passphrase) => bytesToB16(blake2b(passphrase));
exports.encryptPassphrase = encryptPassphrase;
// string utils
const getContentLength = (
  content // 'TextEncoder' is used to measure correct length of UTF-8 strings
) => new TextEncoder().encode(content).length;
exports.getContentLength = getContentLength;
// legacy wallet ID utils
const LEGACY_WALLET_ID_PREFIX = 'legacy_';
const getLegacyWalletId = (rawWalletId) =>
  `${LEGACY_WALLET_ID_PREFIX}${rawWalletId}`;
exports.getLegacyWalletId = getLegacyWalletId;
const getRawWalletId = (legacyWalletId) =>
  legacyWalletId.replace(LEGACY_WALLET_ID_PREFIX, '');
exports.getRawWalletId = getRawWalletId;
const isLegacyWalletId = (walletId) =>
  walletId.startsWith(LEGACY_WALLET_ID_PREFIX);
exports.isLegacyWalletId = isLegacyWalletId;
//# sourceMappingURL=index.js.map
