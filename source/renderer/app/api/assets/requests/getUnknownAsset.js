'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getUnknownAsset = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const getUnknownAsset = (config, { walletId, policyId }) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/${
      (0, utils_1.isLegacyWalletId)(walletId) ? 'byron-wallets' : 'wallets'
    }/${(0, utils_1.getRawWalletId)(walletId)}/assets/${policyId}`,
    ...config,
  });
exports.getUnknownAsset = getUnknownAsset;
//# sourceMappingURL=getUnknownAsset.js.map
