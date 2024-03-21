'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getAddress = void 0;
const request_1 = require('../../utils/request');
const getAddress = (config, { address }) => {
  const encodedAddress = encodeURIComponent(address);
  return (0, request_1.request)({
    method: 'GET',
    path: `/api/v1/addresses/${encodedAddress}`,
    ...config,
  });
};
exports.getAddress = getAddress;
//# sourceMappingURL=getAddress.js.map
