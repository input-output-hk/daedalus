// @flow
import { request } from '../../utils/request';
import type { RequestConfig } from './types';

export const getNextNodeUpdate = (
  config: RequestConfig
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/internal/next-update',
    ...config,
  })
);
