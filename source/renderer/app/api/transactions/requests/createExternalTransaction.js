'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.createExternalTransaction = void 0;
const request_1 = require('../../utils/request');
const createExternalTransaction = (config, { signedTransactionBlob }) =>
  (0, request_1.request)(
    {
      method: 'POST',
      path: '/v2/proxy/transactions',
      ...config,
    },
    {},
    signedTransactionBlob,
    {
      isOctetStreamRequest: true,
    }
  );
exports.createExternalTransaction = createExternalTransaction;
//# sourceMappingURL=createExternalTransaction.js.map
