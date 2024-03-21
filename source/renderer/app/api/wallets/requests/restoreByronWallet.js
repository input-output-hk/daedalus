'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.restoreByronWallet = void 0;
const request_1 = require('../../utils/request');
const restoreByronWallet = (config, { walletInitData }, type) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: '/v2/byron-wallets',
      ...config,
    },
    {},
    { ...walletInitData, style: type }
  );
exports.restoreByronWallet = restoreByronWallet;
//# sourceMappingURL=restoreByronWallet.js.map
