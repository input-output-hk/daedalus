// @flow

export type WalletRecoveryPhraseVerificationStatusesType =
  | 'ok'
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
