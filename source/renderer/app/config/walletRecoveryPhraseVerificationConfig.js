// @flow
import type {
  WalletRecoveryPhraseVerificationStatusesType as VerificationStatus,
  WalletRecoveryPhraseVerificationTypesType as VerificationType,
} from '../types/walletRecoveryPhraseVerificationTypes';

export const RECOVERY_PHRASE_VERIFICATION_STATUSES: {
  [key: string]: VerificationStatus,
} = {
  OK: 'ok',
  OK_TIME_UNTIL: 'okTimeUntil',
  OK_FEW_MONTHS: 'okFewMonths',
  OK_FEW_WEEKS: 'okFewWeeks',
  OK_FEW_DAYS: 'okFewDays',
  WARNING: 'warning',
  NOTIFICATION: 'notification',
};

export const RECOVERY_PHRASE_VERIFICATION_TYPES: {
  [key: string]: VerificationType,
} = {
  NEVER_CHECKED: 'neverChecked',
  ALREADY_CHECKED: 'alreadyChecked',
};

export const RECOVERY_PHRASE_VERIFICATION_OK_FEW_MONTHS: number = 91; // days
export const RECOVERY_PHRASE_VERIFICATION_OK_FEW_WEEKS: number = 153; // days
export const RECOVERY_PHRASE_VERIFICATION_OK_FEW_DAYS: number = 176; // days
export const RECOVERY_PHRASE_VERIFICATION_WARNING: number = 183; // days
export const RECOVERY_PHRASE_VERIFICATION_NOTIFICATION: number = 365; // days
