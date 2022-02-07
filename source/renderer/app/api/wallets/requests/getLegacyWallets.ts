import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallets } from '../types';
import { request } from '../../utils/request';

export const getLegacyWallets = (
  config: RequestConfig
): Promise<LegacyAdaWallets> =>
  request({
    method: 'GET',
    path: '/v2/byron-wallets',
    ...config,
  });
