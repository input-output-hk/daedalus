export type walletBackupStep =
  | 'privacyWarning'
  | 'recoveryPhraseDisplay'
  | 'recoveryPhraseEntry'
  | null;
export const WALLET_BACKUP_STEPS = {
  NOT_INITIATED: null,
  PRIVACY_WARNING: 'privacyWarning',
  RECOVERY_PHRASE_DISPLAY: 'recoveryPhraseDisplay',
  RECOVERY_PHRASE_ENTRY: 'recoveryPhraseEntry',
};
