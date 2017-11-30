// @flow
import type { AdaSyncProgressResponse } from './types';
import { request } from './lib/request';

export type GetAdaSyncProgressParams = {
  ca: string,
};

export const getAdaSyncProgress = (
  { ca }: GetAdaSyncProgressParams
): Promise<AdaSyncProgressResponse> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/sync/progress',
    port: 8090,
    ca,
  })
);
