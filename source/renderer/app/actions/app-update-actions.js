'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action')); // ======= APP UPDATE ACTIONS =======
class AppUpdateActions {
  installUpdate = new Action_1.default();
  postponeUpdate = new Action_1.default();
  openAppUpdateOverlay = new Action_1.default();
  closeAppUpdateOverlay = new Action_1.default();
}
exports.default = AppUpdateActions;
//# sourceMappingURL=app-update-actions.js.map
