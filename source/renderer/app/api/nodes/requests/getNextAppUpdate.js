// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

export const getNextAppUpdate = (config: RequestConfig): Promise<any> =>
  request({
    method: 'GET',
    path: '/api/internal/next-update',
    ...config,
  });
