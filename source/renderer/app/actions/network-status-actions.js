'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action')); // ======= NETWORK STATUS ACTIONS =======
class NetworkStatusActions {
  isSyncedAndReady = new Action_1.default();
  tlsConfigIsReady = new Action_1.default();
  restartNode = new Action_1.default();
  toggleSplash = new Action_1.default();
  copyStateDirectoryPath = new Action_1.default();
  forceCheckNetworkClock = new Action_1.default();
  toggleRTSFlagsMode = new Action_1.default();
}
exports.default = NetworkStatusActions;
//# sourceMappingURL=network-status-actions.js.map
