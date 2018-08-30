// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export const nextAdaUpdate = (
  config: RequestConfig,
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/update',
    port: config.port,
    ca: config.ca,
  })
);
