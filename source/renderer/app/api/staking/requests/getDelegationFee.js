'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getDelegationFee = void 0;
const request_1 = require('../../utils/request');
const getDelegationFee = (config, { walletId }) =>
  (0, request_1.request)({
    method: 'GET',
    path: `/v2/wallets/${walletId}/delegation-fees`,
    ...config,
  });
exports.getDelegationFee = getDelegationFee;
//# sourceMappingURL=getDelegationFee.js.map
