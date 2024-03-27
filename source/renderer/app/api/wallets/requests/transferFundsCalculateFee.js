'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.transferFundsCalculateFee = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const transferFundsCalculateFee = (config, { sourceWalletId }) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(
      sourceWalletId
    )}/migrations`,
    ...config,
  });
exports.transferFundsCalculateFee = transferFundsCalculateFee;
//# sourceMappingURL=transferFundsCalculateFee.js.map
