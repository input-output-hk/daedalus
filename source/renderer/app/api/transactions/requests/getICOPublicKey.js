'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getICOPublicKey = void 0;
const request_1 = require('../../utils/request');
const getICOPublicKey = (config, { walletId, index, data }) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/keys/${index}`,
      ...config,
    },
    {},
    { ...data }
  );
exports.getICOPublicKey = getICOPublicKey;
//# sourceMappingURL=getICOPublicKey.js.map
