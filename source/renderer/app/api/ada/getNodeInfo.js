// @flow
import type { GetNodeInfoResponse } from './types';
import { request } from './lib/v1/request';

export type GetNodeInfoParams = {
  ca: string,
};

export const getNodeInfo = (
  { ca }: GetNodeInfoParams
): Promise<GetNodeInfoResponse> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/node-info',
    port: 8090,
    ca,
  })
);
