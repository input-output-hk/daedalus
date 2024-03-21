'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.RECOVERY_PHRASE_VERIFICATION_TIMES = exports.RECOVERY_PHRASE_VERIFICATION_TYPES = exports.RECOVERY_PHRASE_VERIFICATION_STATUSES = void 0;
exports.RECOVERY_PHRASE_VERIFICATION_STATUSES = {
  OK: 'ok',
  OK_TIME_UNTIL: 'okTimeUntil',
  OK_FEW_MONTHS: 'okFewMonths',
  OK_FEW_WEEKS: 'okFewWeeks',
  OK_FEW_DAYS: 'okFewDays',
  WARNING: 'warning',
  NOTIFICATION: 'notification',
};
exports.RECOVERY_PHRASE_VERIFICATION_TYPES = {
  NEVER_VERIFIED: 'neverVerified',
  ALREADY_VERIFIED: 'alreadyVerified',
};
exports.RECOVERY_PHRASE_VERIFICATION_TIMES = {
  okFewMonths: 91,
  okFewWeeks: 153,
  okFewDays: 176,
  warning: 183,
  notification: 365,
};
//# sourceMappingURL=walletRecoveryPhraseVerificationConfig.js.map
