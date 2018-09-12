// @flow
import { request } from '../../utils/request';
import type { RequestConfig } from './types';

export const postponeNodeUpdate = (
  config: RequestConfig
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/internal/postpone-update',
    ...config,
  })
);
