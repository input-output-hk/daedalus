// @flow
import type { RequestConfig, RedeemAdaParams, AdaTransaction } from './types';
import { request } from '../../utils/request';

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
