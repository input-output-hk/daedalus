'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getTransactionHistory = void 0;
const request_1 = require('../../utils/request');
const getTransactionHistory = (config, walletId, { ...queryParams }) =>
  (0, request_1.request)(
    {
      method: 'GET',
      path: `/v2/wallets/${walletId}/transactions`,
      ...config,
    },
    queryParams,
    null
  );
exports.getTransactionHistory = getTransactionHistory;
//# sourceMappingURL=getTransactionHistory.js.map
