'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getLegacyWallets = void 0;
const request_1 = require('../../utils/request');
const getLegacyWallets = (config) =>
  (0, request_1.request)({
    method: 'GET',
    path: '/v2/byron-wallets',
    ...config,
  });
exports.getLegacyWallets = getLegacyWallets;
//# sourceMappingURL=getLegacyWallets.js.map
