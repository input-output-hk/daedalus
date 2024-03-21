'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.deleteLegacyWallet = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const deleteLegacyWallet = (config, { walletId }) =>
  (0, request_1.request)({
    method: 'DELETE',
    path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(walletId)}`,
    ...config,
  });
exports.deleteLegacyWallet = deleteLegacyWallet;
//# sourceMappingURL=deleteLegacyWallet.js.map
