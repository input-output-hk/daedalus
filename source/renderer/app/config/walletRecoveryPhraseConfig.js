// @flow

import type { WalletRecoveryPhraseStatus } from '../types/walletRecoveryPhraseStatusTypes';

export const WALLET_RECOVERY_PHRASE_STATUSES: {
  NOT_CHECKED: WalletRecoveryPhraseStatus,
  CHECKING: WalletRecoveryPhraseStatus,
  CORRECT: WalletRecoveryPhraseStatus,
  INCORRECT: WalletRecoveryPhraseStatus,
} = {
  NOT_CHECKED: 'notChecked',
  CHECKING: 'checking',
  CORRECT: 'correct',
  INCORRECT: 'incorrect',
};
