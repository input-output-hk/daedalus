// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export const postponeAdaUpdate = (
  config: RequestConfig
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/postpone',
    port: config.port,
    ca: config.ca,
  })
);
