'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getWallet = void 0;
const request_1 = require('../../utils/request');
const getWallet = (config, { walletId }) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/wallets/${walletId}`,
    ...config,
  });
exports.getWallet = getWallet;
//# sourceMappingURL=getWallet.js.map
