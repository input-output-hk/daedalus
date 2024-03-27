'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getNetworkParameters = void 0;
const request_1 = require('../../utils/request');
const getNetworkParameters = (config) =>
  (0, request_1.request)({
    method: 'GET',
    path: '/v2/network/parameters',
    ...config,
  });
exports.getNetworkParameters = getNetworkParameters;
//# sourceMappingURL=getNetworkParameters.js.map
