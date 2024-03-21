'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class ProfileActions {
  acceptAnalytics = new Action_1.default();
  acceptTermsOfUse = new Action_1.default();
  acceptDataLayerMigration = new Action_1.default();
  getLogs = new Action_1.default();
  getLogsAndCompress = new Action_1.default();
  resetBugReportDialog = new Action_1.default();
  downloadLogs = new Action_1.default();
  downloadLogsSuccess = new Action_1.default();
  updateUserLocalSetting = new Action_1.default();
  updateTheme = new Action_1.default();
  finishInitialScreenSettings = new Action_1.default();
  acknowledgeRTSModeRecommendation = new Action_1.default();
}
exports.default = ProfileActions;
//# sourceMappingURL=profile-actions.js.map
