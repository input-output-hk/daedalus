'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.updateSpendingPassword = void 0;
const request_1 = require('../../utils/request');
const updateSpendingPassword = (
  config,
  { walletId, oldPassword, newPassword }
) =>
  (0, request_1.request)(
    {
      method: 'PUT',
      path: `/v2/wallets/${walletId}/passphrase`,
      ...config,
    },
    {},
    {
      old_passphrase: oldPassword,
      new_passphrase: newPassword,
    }
  );
exports.updateSpendingPassword = updateSpendingPassword;
//# sourceMappingURL=updateSpendingPassword.js.map
