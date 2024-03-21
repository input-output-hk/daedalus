'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class WindowActions {
  resizeWindow = new Action_1.default();
  closeWindow = new Action_1.default();
}
exports.default = WindowActions;
//# sourceMappingURL=window-actions.js.map
