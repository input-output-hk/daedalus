'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getAccountPublicKey = void 0;
const request_1 = require('../../utils/request');
const getAccountPublicKey = (
  config,
  { walletId, index, passphrase, extended }
) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/keys/${index}`,
      ...config,
    },
    {},
    {
      passphrase,
      format: extended ? 'extended' : 'non_extended',
    }
  );
exports.getAccountPublicKey = getAccountPublicKey;
//# sourceMappingURL=getAccountPublicKey.js.map
