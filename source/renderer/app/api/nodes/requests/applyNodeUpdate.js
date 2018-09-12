// @flow
import { request } from '../../utils/request';
import type { RequestConfig } from './types';

export const applyNodeUpdate = (
  config: RequestConfig,
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/internal/apply-update',
    ...config,
  })
);
