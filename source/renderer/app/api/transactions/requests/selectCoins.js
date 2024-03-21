'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.selectCoins = void 0;
const request_1 = require('../../utils/request');
const selectCoins = (config, { walletId, data }) => {
  return (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/coin-selections/random`,
      ...config,
    },
    {},
    data
  );
};
exports.selectCoins = selectCoins;
//# sourceMappingURL=selectCoins.js.map
