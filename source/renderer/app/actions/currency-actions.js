'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action')); // ======= CURRENCY ACTIONS =======
class CurrencyActions {
  setCurrencySelected = new Action_1.default();
  toggleCurrencyIsActive = new Action_1.default();
}
exports.default = CurrencyActions;
//# sourceMappingURL=currency-actions.js.map
