'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.createByronWalletTransaction = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const createByronWalletTransaction = (config, { walletId, data }) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(
        walletId
      )}/transactions/`,
      ...config,
    },
    {},
    data
  );
exports.createByronWalletTransaction = createByronWalletTransaction;
//# sourceMappingURL=createByronWalletTransaction.js.map
