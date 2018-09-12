// @flow
import { request } from '../../utils/request';
import type { RequestConfig } from './types';

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
