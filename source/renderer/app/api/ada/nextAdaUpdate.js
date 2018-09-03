// @flow
import { request } from './lib/v1/request';
import type { RequestConfig } from './types';

export const nextAdaUpdate = (
  config: RequestConfig
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/node-settings',
    ...config,
  })
);
