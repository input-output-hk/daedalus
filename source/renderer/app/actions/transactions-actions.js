'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const Action_1 = __importDefault(require('./lib/Action'));
class TransactionsActions {
  filterTransactions = new Action_1.default();
  loadMoreTransactions = new Action_1.default();
  requestCSVFile = new Action_1.default();
  requestCSVFileSuccess = new Action_1.default();
}
exports.default = TransactionsActions;
//# sourceMappingURL=transactions-actions.js.map
