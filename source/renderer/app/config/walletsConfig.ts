import {
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
  PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from './cryptoConfig';

export const CREATE_WALLET_STEPS = [
  'instructions',
  'template',
  'mnemonics',
  'validate',
  'hashImage',
  'config',
];
export const WALLET_RESTORE_TYPES = {
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
export const RECOVERY_PHRASE_WORD_COUNT_OPTIONS = {
  [WALLET_RESTORE_TYPES.REGULAR]: WALLET_RECOVERY_PHRASE_WORD_COUNT,
  [WALLET_RESTORE_TYPES.CERTIFICATE]: PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  [WALLET_RESTORE_TYPES.LEGACY]: LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  [WALLET_RESTORE_TYPES.YOROI_REGULAR]: YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  [WALLET_RESTORE_TYPES.YOROI_LEGACY]: YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT,
};
export const WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH = 15;
export const IS_WALLET_PUBLIC_KEY_SHARING_ENABLED = true;
export const IS_ICO_PUBLIC_KEY_SHARING_ENABLED = true;
export const WALLET_PUBLIC_KEY_DERIVATION_PATH = "M/1852'/1815'/0'";
export const ICO_PUBLIC_KEY_DERIVATION_PATH = "M/1854'/1815'/0'";
// Automatic wallet migration from pre Daedalus 1.0.0 versions has been disabled
export const IS_AUTOMATIC_WALLET_MIGRATION_ENABLED = false;
// Wallet assets feature toggle enable/disable
export const WALLET_ASSETS_ENABLED = true;
// Byron wallet migration has been temporarily disabled due to missing Api support after Mary HF
export const IS_BYRON_WALLET_MIGRATION_ENABLED = false;
export const IS_WALLET_UNDELEGATION_ENABLED = false;
export const TRANSACTION_MIN_ADA_VALUE = 1;
// This should be disabled as long as we don't have DAPP transfer request done
export const IS_DAPP_ENABLED = false;
