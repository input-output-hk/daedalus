// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export const nextAdaUpdate = (
  config: RequestConfig,
): Promise<any> => (
  request(Object.assign({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/update',
  }, config))
);
