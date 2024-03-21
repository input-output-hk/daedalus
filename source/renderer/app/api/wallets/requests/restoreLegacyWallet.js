'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.restoreLegacyWallet = void 0;
const request_1 = require('../../utils/request');
const restoreLegacyWallet = (config, { walletInitData }, type = '') => {
  const queryParams = {};
  return (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/byron-wallets${type}`,
      ...config,
    },
    queryParams,
    walletInitData
  );
};
exports.restoreLegacyWallet = restoreLegacyWallet;
//# sourceMappingURL=restoreLegacyWallet.js.map
