'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.WALLET_BYRON_KINDS = exports.WALLET_HARDWARE_WORD_COUNT = exports.WALLET_YOROI_WORD_COUNT = exports.WALLET_DAEDALUS_WORD_COUNT = exports.WALLET_HARDWARE_KINDS = exports.WALLET_YOROI_KINDS = exports.WALLET_DAEDALUS_KINDS = exports.WALLET_KINDS = exports.IMPORT_WALLET_STEPS = exports.RESTORE_WALLET_STEPS = void 0;
const { isMainnet } = global.environment;
exports.RESTORE_WALLET_STEPS = [
  'type',
  'mnemonics',
  'configuration',
  'success',
];
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.IMPORT_WALLET_STEPS = {
  WALLET_IMPORT_FILE: 'WalletImportFile',
  WALLET_SELECT_IMPORT: 'WalletSelectImport',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.WALLET_KINDS = {
  DAEDALUS: 'Daedalus',
  YOROI: 'Yoroi', // HARDWARE: 'Hardware',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.WALLET_DAEDALUS_KINDS = isMainnet
  ? {
      BYRON_12_WORD: '12WordByron',
      SHELLEY_24_WORD: '24WordShelley',
      BYRON_27_WORD: '27WordPaper',
    }
  : {
      BYRON_12_WORD: '12WordByron',
      SHELLEY_15_WORD: '15WordShelley',
      SHELLEY_24_WORD: '24WordShelley',
      BYRON_27_WORD: '27WordPaper',
    };
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.WALLET_YOROI_KINDS = {
  BYRON_15_WORD: '15WordByron',
  SHELLEY_15_WORD: '15WordShelley',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.WALLET_HARDWARE_KINDS = {
  LEDGER: 'Ledger',
  TREZOR: 'Trezor',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.WALLET_DAEDALUS_WORD_COUNT = {
  [exports.WALLET_DAEDALUS_KINDS.BYRON_12_WORD]: 12,
  [exports.WALLET_DAEDALUS_KINDS.SHELLEY_15_WORD]: 15,
  [exports.WALLET_DAEDALUS_KINDS.SHELLEY_24_WORD]: 24,
  [exports.WALLET_DAEDALUS_KINDS.BYRON_27_WORD]: 27,
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.WALLET_YOROI_WORD_COUNT = {
  [exports.WALLET_YOROI_KINDS.BYRON_15_WORD]: 15,
  [exports.WALLET_YOROI_KINDS.SHELLEY_15_WORD]: 15,
};
// @ts-ignore ts-migrate(2739) FIXME: Type '{ [x: number]: number[]; }' is missing the f... Remove this comment to see the full error message
exports.WALLET_HARDWARE_WORD_COUNT = {
  [exports.WALLET_HARDWARE_KINDS.LEDGER]: [12, 18, 24],
  [exports.WALLET_HARDWARE_KINDS.TREZOR]: [12, 18, 24],
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.WALLET_BYRON_KINDS = {
  ICARUS: 'icarus',
  LEDGER: 'ledger',
  RANDOM: 'random',
  TREZOR: 'trezor',
};
//# sourceMappingURL=walletRestoreConfig.js.map
