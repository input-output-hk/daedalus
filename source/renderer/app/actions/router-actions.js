'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action')); // ======= ROUTER ACTIONS =======
class RouterActions {
  goToRoute = new Action_1.default();
}
exports.default = RouterActions;
//# sourceMappingURL=router-actions.js.map
