'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.transferFunds = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const transferFunds = (
  config,
  { sourceWalletId, targetWalletAddresses, passphrase }
) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(
        sourceWalletId
      )}/migrations`,
      ...config,
    },
    {},
    {
      passphrase,
      addresses: targetWalletAddresses,
    }
  );
exports.transferFunds = transferFunds;
//# sourceMappingURL=transferFunds.js.map
