// @flow
import { request } from './lib/v1/request';
import type { RequestConfig } from './types';

export const applyAdaUpdate = (
  config: RequestConfig,
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/internal/apply-update',
    ...config,
  })
);
