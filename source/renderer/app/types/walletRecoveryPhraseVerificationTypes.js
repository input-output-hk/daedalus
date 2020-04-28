// @flow

export type WalletRecoveryPhraseVerificationStatusesType =
  | 'ok'
  | 'okTimeUntil'
  | 'okFewMonths'
  | 'okFewWeeks'
  | 'okFewDays'
  | 'warning'
  | 'notification';

export type WalletRecoveryPhraseVerificationTypesType =
  | 'neverChecked'
  | 'alreadyChecked';

export type WalletRecoveryPhraseVerificationData = {
  creationDate: ?Date,
  recoveryPhraseVerificationDate: ?Date,
  recoveryPhraseVerificationStatus: string,
  recoveryPhraseVerificationStatusType: string,
};

export type RecoveryPhraseVerificationData = {
  [key: string]: WalletRecoveryPhraseVerificationData,
};
