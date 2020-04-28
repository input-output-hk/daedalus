// @flow

/* eslint-disable no-lonely-if */

import moment from 'moment';
import {
  RECOVERY_PHRASE_VERIFICATION_NOTIFICATION,
  RECOVERY_PHRASE_VERIFICATION_WARNING,
  RECOVERY_PHRASE_VERIFICATION_OK_FEW_MONTHS,
  RECOVERY_PHRASE_VERIFICATION_OK_FEW_WEEKS,
  RECOVERY_PHRASE_VERIFICATION_OK_FEW_DAYS,
  RECOVERY_PHRASE_VERIFICATION_STATUSES,
  RECOVERY_PHRASE_VERIFICATION_TYPES,
} from '../config/walletRecoveryPhraseVerificationConfig';

export const getStatusFromWalletData = ({
  creationDate,
  recoveryPhraseVerificationDate,
}: {
  creationDate: Date,
  recoveryPhraseVerificationDate: ?Date,
}) => {
  // Data config
  const dateToCheck =
    recoveryPhraseVerificationDate || creationDate || new Date();
  const daysSinceDate = moment().diff(moment(dateToCheck), 'days');

  // Status Type
  const recoveryPhraseVerificationStatusType = recoveryPhraseVerificationDate
    ? RECOVERY_PHRASE_VERIFICATION_TYPES.ALREADY_CHECKED
    : RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_CHECKED;
  let recoveryPhraseVerificationStatus;

  // Status
  if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_NOTIFICATION)
    recoveryPhraseVerificationStatus =
      RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION;
  else if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_WARNING)
    recoveryPhraseVerificationStatus =
      RECOVERY_PHRASE_VERIFICATION_STATUSES.WARNING;
  else {
    if (
      recoveryPhraseVerificationStatusType ===
      RECOVERY_PHRASE_VERIFICATION_TYPES.ALREADY_CHECKED
    ) {
      recoveryPhraseVerificationStatus =
        RECOVERY_PHRASE_VERIFICATION_STATUSES.OK;
    } else if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_OK_FEW_DAYS) {
      recoveryPhraseVerificationStatus =
        RECOVERY_PHRASE_VERIFICATION_STATUSES.OK_FEW_DAYS;
    } else if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_OK_FEW_WEEKS) {
      recoveryPhraseVerificationStatus =
        RECOVERY_PHRASE_VERIFICATION_STATUSES.OK_FEW_WEEKS;
    } else if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_OK_FEW_MONTHS) {
      recoveryPhraseVerificationStatus =
        RECOVERY_PHRASE_VERIFICATION_STATUSES.OK_FEW_MONTHS;
    } else {
      recoveryPhraseVerificationStatus =
        RECOVERY_PHRASE_VERIFICATION_STATUSES.OK_TIME_UNTIL;
    }
  }
  return {
    recoveryPhraseVerificationStatus,
    recoveryPhraseVerificationStatusType,
  };
};

export const getWalletType = (recoveryPhraseVerificationDate: ?Date) =>
  recoveryPhraseVerificationDate
    ? RECOVERY_PHRASE_VERIFICATION_TYPES.ALREADY_CHECKED
    : RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_CHECKED;
