// @flow
import type { AdaTransactionV1, AdaTransactionParams } from './types';
import { request } from './lib/v1/request';

export const newAdaPayment = (
  { ca, data }: AdaTransactionParams
): Promise<AdaTransactionV1> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions',
    port: 8090,
    ca,
  }, {}, data)
);
