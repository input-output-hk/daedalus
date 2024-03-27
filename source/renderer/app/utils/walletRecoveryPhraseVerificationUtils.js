'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getStatusFromWalletData = void 0;
const moment_1 = __importDefault(require('moment'));
const walletRecoveryPhraseVerificationConfig_1 = require('../config/walletRecoveryPhraseVerificationConfig');
const getStatusFromWalletData = ({
  creationDate: creation,
  recoveryPhraseVerificationDate: verification,
}) => {
  // Data config
  const dateToCheck = verification || creation || new Date();
  const daysSinceDate = (0, moment_1.default)().diff(
    (0, moment_1.default)(dateToCheck),
    'days'
  );
  // Status Type
  const type = verification
    ? walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_TYPES.ALREADY_VERIFIED
    : walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_TYPES.NEVER_VERIFIED;
  // Status
  let status;
  if (
    daysSinceDate >
    walletRecoveryPhraseVerificationConfig_1.RECOVERY_PHRASE_VERIFICATION_TIMES
      .notification
  )
    status =
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION;
  else if (
    daysSinceDate >
    walletRecoveryPhraseVerificationConfig_1.RECOVERY_PHRASE_VERIFICATION_TIMES
      .warning
  )
    status =
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_STATUSES.WARNING;
  else if (
    type ===
    walletRecoveryPhraseVerificationConfig_1.RECOVERY_PHRASE_VERIFICATION_TYPES
      .ALREADY_VERIFIED
  )
    status =
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_STATUSES.OK;
  else if (
    daysSinceDate >
    walletRecoveryPhraseVerificationConfig_1.RECOVERY_PHRASE_VERIFICATION_TIMES
      .okFewDays
  )
    status =
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_STATUSES.OK_FEW_DAYS;
  else if (
    daysSinceDate >
    walletRecoveryPhraseVerificationConfig_1.RECOVERY_PHRASE_VERIFICATION_TIMES
      .okFewWeeks
  )
    status =
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_STATUSES.OK_FEW_WEEKS;
  else if (
    daysSinceDate >
    walletRecoveryPhraseVerificationConfig_1.RECOVERY_PHRASE_VERIFICATION_TIMES
      .okFewMonths
  )
    status =
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_STATUSES.OK_FEW_MONTHS;
  else
    status =
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_STATUSES.OK_TIME_UNTIL;
  return {
    recoveryPhraseVerificationStatus: status,
    recoveryPhraseVerificationStatusType: type,
  };
};
exports.getStatusFromWalletData = getStatusFromWalletData;
//# sourceMappingURL=walletRecoveryPhraseVerificationUtils.js.map
