'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getWalletPublicKey = void 0;
const request_1 = require('../../utils/request');
const getWalletPublicKey = (config, { walletId, role, index }) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/wallets/${walletId}/keys/${role}/${index}`,
    ...config,
  });
exports.getWalletPublicKey = getWalletPublicKey;
//# sourceMappingURL=getWalletPublicKey.js.map
