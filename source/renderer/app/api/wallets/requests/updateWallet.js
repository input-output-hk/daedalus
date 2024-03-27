'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.updateWallet = void 0;
const request_1 = require('../../utils/request');
const updateWallet = (config, { walletId, name }) =>
  (0, request_1.request)(
    {
      method: 'PUT',
      path: `/v2/wallets/${walletId}`,
      ...config,
    },
    {},
    {
      name,
    }
  );
exports.updateWallet = updateWallet;
//# sourceMappingURL=updateWallet.js.map
