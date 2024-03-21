'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.TransactionInfo = exports.TransactionsGroup = void 0;
class TransactionsGroup {
  date;
  transactions;
  constructor(props) {
    Object.assign(this, props);
  }
}
exports.TransactionsGroup = TransactionsGroup;
class TransactionInfo {
  tx;
  isLastInGroup;
  isFirstInGroup;
  constructor(props) {
    Object.assign(this, props);
  }
}
exports.TransactionInfo = TransactionInfo;
//# sourceMappingURL=types.js.map
