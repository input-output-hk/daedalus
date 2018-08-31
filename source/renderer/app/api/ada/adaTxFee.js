// @flow
import type { AdaTransactionFee, AdaTxFeeParams, RequestConfig } from './types';
import { request } from './lib/v1/request';

export const adaTxFee = (
  config: RequestConfig,
  { data }: AdaTxFeeParams
): Promise<AdaTransactionFee> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/fees',
    ...config,
  }, {}, data)
);
