'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.createTransaction = void 0;
const request_1 = require('../../utils/request');
const createTransaction = (config, { walletId, data }) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/transactions/`,
      ...config,
    },
    {},
    data
  );
exports.createTransaction = createTransaction;
//# sourceMappingURL=createTransaction.js.map
