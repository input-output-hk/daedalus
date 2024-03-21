'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.TRANSACTION_MIN_ADA_VALUE = exports.IS_WALLET_UNDELEGATION_ENABLED = exports.IS_BYRON_WALLET_MIGRATION_ENABLED = exports.WALLET_ASSETS_ENABLED = exports.IS_AUTOMATIC_WALLET_MIGRATION_ENABLED = exports.ICO_PUBLIC_KEY_DERIVATION_PATH = exports.WALLET_PUBLIC_KEY_DERIVATION_PATH = exports.IS_ICO_PUBLIC_KEY_SHARING_ENABLED = exports.IS_WALLET_PUBLIC_KEY_SHARING_ENABLED = exports.WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH = exports.RECOVERY_PHRASE_WORD_COUNT_OPTIONS = exports.WALLET_RESTORE_TYPES = exports.CREATE_WALLET_STEPS = void 0;
const cryptoConfig_1 = require('./cryptoConfig');
exports.CREATE_WALLET_STEPS = [
  'instructions',
  'template',
  'mnemonics',
  'validate',
  'hashImage',
  'config',
];
exports.WALLET_RESTORE_TYPES = {
  REGULAR: 'regular',
  // Shelley wallet
  CERTIFICATE: 'certificate',
  // Paper wallet
  LEGACY: 'legacy',
  // Byron wallet
  YOROI_REGULAR: 'yoroi-regular',
  // Yoroi regular (rewards) wallet
  YOROI_LEGACY: 'yoroi-legacy', // Yoroi legacy (balance) wallet
};
exports.RECOVERY_PHRASE_WORD_COUNT_OPTIONS = {
  [exports.WALLET_RESTORE_TYPES.REGULAR]:
    cryptoConfig_1.WALLET_RECOVERY_PHRASE_WORD_COUNT,
  [exports.WALLET_RESTORE_TYPES.CERTIFICATE]:
    cryptoConfig_1.PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  [exports.WALLET_RESTORE_TYPES.LEGACY]:
    cryptoConfig_1.LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  [exports.WALLET_RESTORE_TYPES.YOROI_REGULAR]:
    cryptoConfig_1.YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  [exports.WALLET_RESTORE_TYPES.YOROI_LEGACY]:
    cryptoConfig_1.YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT,
};
exports.WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH = 15;
exports.IS_WALLET_PUBLIC_KEY_SHARING_ENABLED = true;
exports.IS_ICO_PUBLIC_KEY_SHARING_ENABLED = true;
exports.WALLET_PUBLIC_KEY_DERIVATION_PATH = "M/1852'/1815'/0'";
exports.ICO_PUBLIC_KEY_DERIVATION_PATH = "M/1854'/1815'/0'";
// Automatic wallet migration from pre Daedalus 1.0.0 versions has been disabled
exports.IS_AUTOMATIC_WALLET_MIGRATION_ENABLED = false;
// Wallet assets feature toggle enable/disable
exports.WALLET_ASSETS_ENABLED = true;
// Byron wallet migration has been temporarily disabled due to missing Api support after Mary HF
exports.IS_BYRON_WALLET_MIGRATION_ENABLED = false;
exports.IS_WALLET_UNDELEGATION_ENABLED = false;
exports.TRANSACTION_MIN_ADA_VALUE = 1;
//# sourceMappingURL=walletsConfig.js.map
