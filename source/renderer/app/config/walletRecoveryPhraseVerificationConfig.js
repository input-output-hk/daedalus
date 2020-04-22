// @flow
import type {
  WalletRecoveryPhraseVerificationStatusesType as VerificationStatus,
  WalletRecoveryPhraseVerificationTypesType as VerificationType,
} from '../types/walletRecoveryPhraseVerificationTypes';

export const RECOVERY_PHRASE_VERIFICATION_STATUSES: {
  OK: VerificationStatus,
  WARNING: VerificationStatus,
  NOTIFICATION: VerificationStatus,
} = {
  OK: 'ok',
  WARNING: 'warning',
  NOTIFICATION: 'notification',
};

export const WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES: {
  NEVER_CHECKED: VerificationType,
  ALREADY_CHECKED: VerificationType,
} = {
  NEVER_CHECKED: 'neverChecked',
  ALREADY_CHECKED: 'alreadyChecked',
};

export const RECOVERY_PHRASE_VERIFICATION_WARNING: number = 183; // days
export const RECOVERY_PHRASE_VERIFICATION_NOTIFICATION: number = 365; // days
