'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class WalletSettingsActions {
  refreshWalletsLocalData = new Action_1.default();
  setWalletLocalData = new Action_1.default();
  unsetWalletLocalData = new Action_1.default();
}
exports.default = WalletSettingsActions;
//# sourceMappingURL=wallets-local-actions.js.map
