'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getTransaction = void 0;
const request_1 = require('../../utils/request');
const getTransaction = (config, walletId, transactionId) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/wallets/${walletId}/transactions/${transactionId}`,
    ...config,
  });
exports.getTransaction = getTransaction;
//# sourceMappingURL=getTransaction.js.map
