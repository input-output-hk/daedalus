// @flow
import type { AdaTransactionV1, RequestConfig } from './types';
import type { RedeemAdaRequest } from './';
import { request } from './lib/v1/request';

export const redeemAda = (
  config: RequestConfig,
  redemptionData: RedeemAdaRequest
): Promise<AdaTransactionV1> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    ...config
  }, {}, redemptionData)
);
