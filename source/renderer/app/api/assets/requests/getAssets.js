'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getAssets = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const getAssets = (config, { walletId }) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/${
      (0, utils_1.isLegacyWalletId)(walletId) ? 'byron-wallets' : 'wallets'
    }/${(0, utils_1.getRawWalletId)(walletId)}/assets`,
    ...config,
  });
exports.getAssets = getAssets;
//# sourceMappingURL=getAssets.js.map
