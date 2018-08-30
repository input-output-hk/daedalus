// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export const adaTestReset = (
  config: RequestConfig
): Promise<void> => (
  request(Object.assign({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/test/reset',
  }, config))
);
