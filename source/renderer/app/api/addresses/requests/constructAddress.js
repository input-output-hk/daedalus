'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.constructAddress = void 0;
const request_1 = require('../../utils/request');
const constructAddress = (
  config,
  { data } // @TODO
) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: '/v2/addresses',
      ...config,
    },
    {},
    data
  );
exports.constructAddress = constructAddress;
//# sourceMappingURL=constructAddress.js.map
