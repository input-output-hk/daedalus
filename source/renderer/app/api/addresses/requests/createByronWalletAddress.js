'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.createByronWalletAddress = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const createByronWalletAddress = (
  config,
  { passphrase, addressIndex, walletId }
) => {
  let data = {
    passphrase,
  };
  // @ts-ignore ts-migrate(2322) FIXME: Type '{ passphrase: string; } | { address_index: n... Remove this comment to see the full error message
  data = addressIndex ? { ...data, address_index: addressIndex } : data;
  return (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(
        walletId
      )}/addresses`,
      ...config,
    },
    {},
    data
  );
};
exports.createByronWalletAddress = createByronWalletAddress;
//# sourceMappingURL=createByronWalletAddress.js.map
