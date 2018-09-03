// @flow
import type { NodeInfo, RequestConfig } from './types';
import { request } from './lib/v1/request';

export const getNodeInfo = (
  config: RequestConfig
): Promise<NodeInfo> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/node-info',
    ...config,
  })
);
