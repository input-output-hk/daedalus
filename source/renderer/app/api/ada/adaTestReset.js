// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export const adaTestReset = (
  config: RequestConfig
): Promise<void> => (
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: '/api/internal/reset-wallet-state',
    ...config,
  })
);
