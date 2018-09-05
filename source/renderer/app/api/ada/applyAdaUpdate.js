// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export const applyAdaUpdate = (
  config: RequestConfig,
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/apply',
    ...config,
  })
);
