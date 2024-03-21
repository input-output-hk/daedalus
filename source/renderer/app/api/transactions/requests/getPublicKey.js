'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getPublicKey = void 0;
const request_1 = require('../../utils/request');
const getPublicKey = (
  config,
  { walletId, role, index } // @TODO
) =>
  (0, request_1.request)(
    {
      method: 'GET',
      path: `/v2/wallets/${walletId}/keys/${role}/${index}`,
      ...config,
    },
    {},
    {}
  );
exports.getPublicKey = getPublicKey;
//# sourceMappingURL=getPublicKey.js.map
