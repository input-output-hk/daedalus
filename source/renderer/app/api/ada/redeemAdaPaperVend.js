// @flow
import type { RequestConfig, RedeemPaperVendedAdaParams, AdaTransaction } from './types';
import { request } from './lib/v1/request';

export const redeemAdaPaperVend = (
  config: RequestConfig,
  redemptionParams: RedeemPaperVendedAdaParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    ...config
  }, {}, redemptionParams)
);
