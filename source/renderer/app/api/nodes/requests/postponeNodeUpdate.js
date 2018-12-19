// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

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
