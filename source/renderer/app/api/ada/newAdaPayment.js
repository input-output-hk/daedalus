// @flow
import type { AdaTransactionV1, AdaTransactionParams, RequestConfig } from './types';
import { request } from './lib/v1/request';

export const newAdaPayment = (
  config: RequestConfig,
  { data }: AdaTransactionParams
): Promise<AdaTransactionV1> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions',
    ...config,
  }, {}, data)
);
