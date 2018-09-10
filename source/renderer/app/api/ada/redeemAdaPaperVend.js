// @flow
import type { RequestConfig, RedeemPaperVendedAdaParams, AdaTransactionV1 } from './types';
import { request } from './lib/v1/request';

export const redeemAdaPaperVend = (
  config: RequestConfig,
  redemptionParams: RedeemPaperVendedAdaParams
): Promise<AdaTransactionV1> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    ...config
  }, {}, redemptionParams)
);
