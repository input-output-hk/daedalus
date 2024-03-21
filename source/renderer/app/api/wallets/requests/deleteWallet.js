'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.deleteWallet = void 0;
const request_1 = require('../../utils/request');
const deleteWallet = (config, { walletId }) =>
  (0, request_1.request)({
    method: 'DELETE',
    path: `/v2/wallets/${walletId}`,
    ...config,
  });
exports.deleteWallet = deleteWallet;
//# sourceMappingURL=deleteWallet.js.map
