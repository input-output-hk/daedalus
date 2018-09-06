// @flow
import { request } from './lib/v1/request';
import type { RequestConfig } from './types';

export const postponeAdaUpdate = (
  config: RequestConfig
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/internal/postpone-update',
    ...config,
  })
);
