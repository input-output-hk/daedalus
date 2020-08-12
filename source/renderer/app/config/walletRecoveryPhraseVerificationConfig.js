// @flow
import type {
  WalletRecoveryPhraseVerificationStatusesType as Status,
  WalletRecoveryPhraseVerificationTypesType as Type,
} from '../types/walletRecoveryPhraseVerificationTypes';

export const RECOVERY_PHRASE_VERIFICATION_STATUSES: {
  [key: string]: Status,
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
  [key: string]: Type,
} = {
  NEVER_VERIFIED: 'neverVerified',
  ALREADY_VERIFIED: 'alreadyVerified',
};

export const RECOVERY_PHRASE_VERIFICATION_TIMES: {
  [key: string]: number,
} = {
  okFewMonths: 91,
  okFewWeeks: 153,
  okFewDays: 176,
  warning: 183,
  notification: 365,
};
