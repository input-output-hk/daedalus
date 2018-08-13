// @flow
import type { AdaTransactionV1, AdaTransactionPayloadV1 } from './types';
import { request } from './lib/v1/request';

export const newAdaPayment = (
  { ca, data }: AdaTransactionPayloadV1
): Promise<AdaTransactionV1> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions',
    port: 8090,
    ca,
  }, {}, data)
);
