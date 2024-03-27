'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getWalletUtxos = void 0;
const request_1 = require('../../utils/request');
const getWalletUtxos = (config, { walletId }) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/wallets/${walletId}/statistics/utxos`,
    ...config,
  });
exports.getWalletUtxos = getWalletUtxos;
//# sourceMappingURL=getWalletUtxos.js.map
