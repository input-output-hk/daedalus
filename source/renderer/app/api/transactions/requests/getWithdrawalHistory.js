'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getWithdrawalHistory = void 0;
const request_1 = require('../../utils/request');
const getWithdrawalHistory = (config, walletId) =>
  (0, request_1.request)(
    {
      method: 'GET',
      path: `/v2/wallets/${walletId}/transactions`,
      ...config,
    },
    {
      minWithdrawal: 1,
    },
    null
  );
exports.getWithdrawalHistory = getWithdrawalHistory;
//# sourceMappingURL=getWithdrawalHistory.js.map
