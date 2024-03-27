'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.quitStakePool = void 0;
const request_1 = require('../../utils/request');
const quitStakePool = (config, { walletId, passphrase }) =>
  (0, request_1.request)(
    {
      method: 'DELETE',
      path: `/v2/stake-pools/*/wallets/${walletId}`,
      ...config,
    },
    {},
    {
      passphrase,
    }
  );
exports.quitStakePool = quitStakePool;
//# sourceMappingURL=quitStakePool.js.map
