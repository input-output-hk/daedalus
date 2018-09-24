// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

export const resetWalletState = (
  config: RequestConfig
): Promise<void> => (
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: '/api/internal/reset-wallet-state',
    ...config,
  })
);
