// @flow
import type { AdaV1Wallets, RequestConfig } from './types';
import { request } from './lib/v1/request';
import { MAX_ADA_WALLETS_COUNT } from '../../config/numbersConfig';

export const getAdaWallets = (
  config: RequestConfig
): Promise<AdaV1Wallets> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/wallets',
    ...config,
  }, {
    per_page: MAX_ADA_WALLETS_COUNT, // 50 is the max per_page value
    sort_by: 'ASC[created_at]',
  })
);
