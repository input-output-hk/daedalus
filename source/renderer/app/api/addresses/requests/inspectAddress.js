'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.inspectAddress = void 0;
const request_1 = require('../../utils/request');
const inspectAddress = (
  config,
  { addressId } // @TODO
) =>
  (0, request_1.request)(
    {
      method: 'GET',
      path: `/v2/addresses/${addressId}`,
      ...config,
    },
    {},
    {}
  );
exports.inspectAddress = inspectAddress;
//# sourceMappingURL=inspectAddress.js.map
