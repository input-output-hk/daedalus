// @flow
import type { RequestConfig } from '../../common/types';
import type { NodeInfo } from '../types';
import { request } from '../../utils/request';

export type NodeQueryParams = {
  force_ntp_check: boolean,
};

export const getNodeInfo = (
  config: RequestConfig,
  queryParams?: NodeQueryParams,
): Promise<NodeInfo> => (
  request({
    method: 'GET',
    path: '/api/v1/node-info',
    ...config,
  }, queryParams)
);
