// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export const adaTestReset = (
  config: RequestConfig
): Promise<void> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/test/reset',
    port: config.port,
    ca: config.ca,
  })
);
