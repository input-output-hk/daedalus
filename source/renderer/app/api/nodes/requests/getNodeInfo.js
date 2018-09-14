// @flow
import type { RequestConfig } from '../../common/types';
import type { NodeInfo } from '../types';
import { request } from '../../utils/request';

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
