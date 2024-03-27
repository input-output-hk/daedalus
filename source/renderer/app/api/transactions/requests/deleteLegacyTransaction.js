'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.deleteLegacyTransaction = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const deleteLegacyTransaction = (config, { walletId, transactionId }) =>
  (0, request_1.request)({
    method: 'DELETE',
    path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(
      walletId
    )}/transactions/${transactionId}`,
    ...config,
  });
exports.deleteLegacyTransaction = deleteLegacyTransaction;
//# sourceMappingURL=deleteLegacyTransaction.js.map
