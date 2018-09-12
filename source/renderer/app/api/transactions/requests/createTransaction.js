// @flow
import type { AdaTransaction, AdaTransactionParams, RequestConfig } from './types';
import { request } from '../../utils/request';

export const createTransaction = (
  config: RequestConfig,
  { data }: AdaTransactionParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions',
    ...config,
  }, {}, data)
);
