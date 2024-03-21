'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getNetworkInfo = void 0;
const request_1 = require('../../utils/request');
const getNetworkInfo = (config) =>
  (0, request_1.request)({
    method: 'GET',
    path: '/v2/network/information',
    ...config,
  });
exports.getNetworkInfo = getNetworkInfo;
//# sourceMappingURL=getNetworkInfo.js.map
