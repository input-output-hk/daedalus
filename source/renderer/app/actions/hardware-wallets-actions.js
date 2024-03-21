'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class HardwareWalletsActions {
  /* ----------  Get hardware wallet device  ---------- */
  selectCoins = new Action_1.default();
  selectDelegationCoins = new Action_1.default();
  sendMoney = new Action_1.default();
  refreshHardwareWalletsLocalData = new Action_1.default();
}
exports.default = HardwareWalletsActions;
//# sourceMappingURL=hardware-wallets-actions.js.map
