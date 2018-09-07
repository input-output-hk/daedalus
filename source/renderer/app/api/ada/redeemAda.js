// @flow
import type { RequestConfig, RedeemAdaParams, AdaTransactionV1 } from './types';
import { request } from './lib/v1/request';

export const redeemAda = (
  config: RequestConfig,
  redemptionParams: RedeemAdaParams
): Promise<AdaTransactionV1> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    ...config
  }, {}, redemptionParams)
);
