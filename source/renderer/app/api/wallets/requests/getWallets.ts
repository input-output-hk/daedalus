import type { RequestConfig } from '../../common/types';
import type { AdaWallets } from '../types';
import { request } from '../../utils/request';

export const getWallets = (config: RequestConfig): Promise<AdaWallets> =>
  request({
    method: 'GET',
    path: '/v2/wallets',
    ...config,
  });
