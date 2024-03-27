'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.createWalletSignature = void 0;
const request_1 = require('../../utils/request');
const createWalletSignature = (config, { walletId, role, index, data }) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/signatures/${role}/${index}`,
      ...config,
    },
    {},
    data,
    {
      isOctetStreamResponse: true,
    }
  );
exports.createWalletSignature = createWalletSignature;
//# sourceMappingURL=createWalletSignature.js.map
