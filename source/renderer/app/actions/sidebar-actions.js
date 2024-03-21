'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action')); // ======= SIDEBAR ACTIONS =======
class SidebarActions {
  showSubMenus = new Action_1.default();
  toggleSubMenus = new Action_1.default();
  activateSidebarCategory = new Action_1.default();
  walletSelected = new Action_1.default();
}
exports.default = SidebarActions;
//# sourceMappingURL=sidebar-actions.js.map
