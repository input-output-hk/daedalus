'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.updateByronWallet = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const updateByronWallet = (config, { walletId, name }) =>
  (0, request_1.request)(
    {
      method: 'PUT',
      path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(walletId)}`,
      ...config,
    },
    {},
    {
      name,
    }
  );
exports.updateByronWallet = updateByronWallet;
//# sourceMappingURL=updateByronWallet.js.map
