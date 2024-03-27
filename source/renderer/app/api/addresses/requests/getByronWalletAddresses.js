'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getByronWalletAddresses = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const getByronWalletAddresses = (config, walletId, queryParams) =>
  (0, request_1.request)(
    {
      method: 'GET',
      path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(
        walletId
      )}/addresses`,
      ...config,
    },
    queryParams
  );
exports.getByronWalletAddresses = getByronWalletAddresses;
//# sourceMappingURL=getByronWalletAddresses.js.map
