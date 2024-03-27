'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getLegacyWallet = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const getLegacyWallet = (config, { walletId }) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(walletId)}`,
    ...config,
  });
exports.getLegacyWallet = getLegacyWallet;
//# sourceMappingURL=getLegacyWallet.js.map
