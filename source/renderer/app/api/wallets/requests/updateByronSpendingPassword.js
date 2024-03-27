'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.updateByronSpendingPassword = void 0;
const request_1 = require('../../utils/request');
const utils_1 = require('../../utils');
const updateByronSpendingPassword = (
  config,
  { walletId, oldPassword, newPassword }
) =>
  (0, request_1.request)(
    {
      method: 'PUT',
      path: `/v2/byron-wallets/${(0, utils_1.getRawWalletId)(
        walletId
      )}/passphrase`,
      ...config,
    },
    {},
    {
      old_passphrase: oldPassword,
      new_passphrase: newPassword,
    }
  );
exports.updateByronSpendingPassword = updateByronSpendingPassword;
//# sourceMappingURL=updateByronSpendingPassword.js.map
