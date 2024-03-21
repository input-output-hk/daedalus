'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.deleteTransaction = void 0;
const request_1 = require('../../utils/request');
const deleteTransaction = (config, { walletId, transactionId }) =>
  (0, request_1.request)({
    method: 'DELETE',
    path: `/v2/wallets/${walletId}/transactions/${transactionId}`,
    ...config,
  });
exports.deleteTransaction = deleteTransaction;
//# sourceMappingURL=deleteTransaction.js.map
