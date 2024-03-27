'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class NotificationsActions {
  registerNotification = new Action_1.default();
  closeActiveNotification = new Action_1.default();
  closeNotification = new Action_1.default();
}
exports.default = NotificationsActions;
//# sourceMappingURL=notifications-actions.js.map
