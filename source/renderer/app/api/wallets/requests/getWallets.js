// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallets } from '../types';
import { request } from '../../utils/request';
import { MAX_ADA_WALLETS_COUNT } from '../../../config/numbersConfig';

export const getWallets = (
  config: RequestConfig
): Promise<AdaWallets> => (
  request({
    method: 'GET',
    path: '/api/v1/wallets',
    ...config,
  }, {
    per_page: MAX_ADA_WALLETS_COUNT, // 50 is the max per_page value
    sort_by: 'ASC[created_at]',
  })
);
