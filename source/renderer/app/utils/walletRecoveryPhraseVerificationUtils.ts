import moment from 'moment';
import {
  RECOVERY_PHRASE_VERIFICATION_STATUSES as statuses,
  RECOVERY_PHRASE_VERIFICATION_TYPES as types,
  RECOVERY_PHRASE_VERIFICATION_TIMES as times,
} from '../config/walletRecoveryPhraseVerificationConfig';

export const getStatusFromWalletData = ({
  creationDate: creation,
  recoveryPhraseVerificationDate: verification,
}: {
  creationDate: Date;
  recoveryPhraseVerificationDate: Date | null | undefined;
}) => {
  // Data config
  const dateToCheck = verification || creation || new Date();
  const daysSinceDate = moment().diff(moment(dateToCheck), 'days');
  // Status Type
  const type = verification ? types.ALREADY_VERIFIED : types.NEVER_VERIFIED;
  // Status
  let status;
  if (daysSinceDate > times.notification) status = statuses.NOTIFICATION;
  else if (daysSinceDate > times.warning) status = statuses.WARNING;
  else if (type === types.ALREADY_VERIFIED) status = statuses.OK;
  else if (daysSinceDate > times.okFewDays) status = statuses.OK_FEW_DAYS;
  else if (daysSinceDate > times.okFewWeeks) status = statuses.OK_FEW_WEEKS;
  else if (daysSinceDate > times.okFewMonths) status = statuses.OK_FEW_MONTHS;
  else status = statuses.OK_TIME_UNTIL;
  return {
    recoveryPhraseVerificationStatus: status,
    recoveryPhraseVerificationStatusType: type,
  };
};
