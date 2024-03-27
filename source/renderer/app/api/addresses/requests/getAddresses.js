'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getAddresses = void 0;
const request_1 = require('../../utils/request');
const getAddresses = (config, walletId, queryParams) =>
  (0, request_1.request)(
    {
      method: 'GET',
      path: `/v2/wallets/${walletId}/addresses`,
      ...config,
    },
    queryParams
  );
exports.getAddresses = getAddresses;
//# sourceMappingURL=getAddresses.js.map
