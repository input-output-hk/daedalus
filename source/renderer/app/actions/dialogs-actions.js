'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action')); // ======= DIALOGS ACTIONS =======
class DialogsActions {
  open = new Action_1.default();
  updateDataForActiveDialog = new Action_1.default();
  closeActiveDialog = new Action_1.default();
  resetActiveDialog = new Action_1.default();
}
exports.default = DialogsActions;
//# sourceMappingURL=dialogs-actions.js.map
