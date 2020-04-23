// @flow
import moment from 'moment';
import {
  RECOVERY_PHRASE_VERIFICATION_NOTIFICATION,
  RECOVERY_PHRASE_VERIFICATION_WARNING,
  RECOVERY_PHRASE_VERIFICATION_STATUSES,
  WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES,
} from '../config/walletRecoveryPhraseVerificationConfig';

export const getStatusFromWalletData = ({
  creationDate,
  recoveryPhraseVerificationDate,
}: {
  creationDate: Date,
  recoveryPhraseVerificationDate: ?Date,
}) => {
  const dateToCheck =
    recoveryPhraseVerificationDate || creationDate || new Date();
  const daysSinceDate = moment().diff(moment(dateToCheck), 'days');
  let recoveryPhraseVerificationStatus =
    RECOVERY_PHRASE_VERIFICATION_STATUSES.OK;
  if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_NOTIFICATION)
    recoveryPhraseVerificationStatus =
      RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION;
  else if (daysSinceDate > RECOVERY_PHRASE_VERIFICATION_WARNING)
    recoveryPhraseVerificationStatus =
      RECOVERY_PHRASE_VERIFICATION_STATUSES.WARNING;
  const recoveryPhraseVerificationStatusType = recoveryPhraseVerificationDate
    ? WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES.ALREADY_CHECKED
    : WALLET_RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_CHECKED;
  return {
    recoveryPhraseVerificationStatus,
    recoveryPhraseVerificationStatusType,
  };
};
