'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getWallets = void 0;
const request_1 = require('../../utils/request');
const getWallets = (config) =>
  (0, request_1.request)({
    method: 'GET',
    path: '/v2/wallets',
    ...config,
  });
exports.getWallets = getWallets;
//# sourceMappingURL=getWallets.js.map
