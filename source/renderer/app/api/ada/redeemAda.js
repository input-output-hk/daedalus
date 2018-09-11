// @flow
import type { RequestConfig, RedeemAdaParams, AdaTransaction } from './types';
import { request } from './lib/v1/request';

export const redeemAda = (
  config: RequestConfig,
  redemptionParams: RedeemAdaParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    ...config
  }, {}, redemptionParams)
);
