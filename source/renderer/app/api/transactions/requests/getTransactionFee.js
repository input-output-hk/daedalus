'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getTransactionFee = void 0;
const request_1 = require('../../utils/request');
const getTransactionFee = (config, { walletId, data }) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/payment-fees`,
      ...config,
    },
    {},
    data
  );
exports.getTransactionFee = getTransactionFee;
//# sourceMappingURL=getTransactionFee.js.map
