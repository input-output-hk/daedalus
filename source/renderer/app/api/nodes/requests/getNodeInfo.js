// @flow
import type { RequestConfig } from '../../common/types';
import type { NodeInfoResponse } from '../types';
import { request } from '../../utils/request';

export type NodeInfoQueryParams = {
  force_ntp_check: boolean,
};

export const getNodeInfo = (
  config: RequestConfig,
  queryInfoParams?: NodeInfoQueryParams
): Promise<NodeInfoResponse> =>
  request(
    {
      method: 'GET',
      path: '/api/v1/node-info',
      ...config,
    },
    queryInfoParams
  );
