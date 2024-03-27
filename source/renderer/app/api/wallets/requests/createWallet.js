'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.createWallet = void 0;
const request_1 = require('../../utils/request');
const createWallet = (config, { walletInitData }) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: '/v2/wallets',
      ...config,
    },
    {},
    walletInitData
  );
exports.createWallet = createWallet;
//# sourceMappingURL=createWallet.js.map
