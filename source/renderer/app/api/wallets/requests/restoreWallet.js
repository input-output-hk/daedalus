'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.restoreWallet = void 0;
const request_1 = require('../../utils/request');
const restoreWallet = (config, { walletInitData }) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: '/v2/wallets',
      ...config,
    },
    {},
    walletInitData
  );
exports.restoreWallet = restoreWallet;
//# sourceMappingURL=restoreWallet.js.map
