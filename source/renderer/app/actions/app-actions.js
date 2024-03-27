'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action')); // ======= APP ACTIONS =======
class AppActions {
  downloadLogs = new Action_1.default();
  getGpuStatus = new Action_1.default();
  initAppEnvironment = new Action_1.default();
  setIsDownloadingLogs = new Action_1.default();
  toggleNewsFeed = new Action_1.default();
  closeNewsFeed = new Action_1.default();
  onUiClicked = new Action_1.default();
  // About dialog actions
  closeAboutDialog = new Action_1.default();
  openAboutDialog = new Action_1.default();
  // Daedalus Diagnostics dialog actions
  closeDaedalusDiagnosticsDialog = new Action_1.default();
  openDaedalusDiagnosticsDialog = new Action_1.default();
  closeToggleRTSFlagsModeDialog = new Action_1.default();
  openToggleRTSFlagsModeDialog = new Action_1.default();
}
exports.default = AppActions;
//# sourceMappingURL=app-actions.js.map
