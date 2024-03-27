'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.joinStakePool = void 0;
const request_1 = require('../../utils/request');
const joinStakePool = (config, { walletId, stakePoolId, passphrase }) =>
  (0, request_1.request)(
    {
      method: 'PUT',
      path: `/v2/stake-pools/${stakePoolId}/wallets/${walletId}`,
      ...config,
    },
    {},
    {
      passphrase,
    }
  );
exports.joinStakePool = joinStakePool;
//# sourceMappingURL=joinStakePool.js.map
