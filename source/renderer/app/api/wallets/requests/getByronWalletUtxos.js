'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getByronWalletUtxos = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const getByronWalletUtxos = (config, { walletId }) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(
      walletId
    )}/statistics/utxos`,
    ...config,
  });
exports.getByronWalletUtxos = getByronWalletUtxos;
//# sourceMappingURL=getByronWalletUtxos.js.map
