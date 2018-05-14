// @flow
import type { AdaSyncProgressResponse } from './types';
import { request } from './lib/request';

export type GetAdaSyncProgressParams = {
  ca: string,
  port: number,
};

export const getAdaSyncProgress = (
  { ca, port }: GetAdaSyncProgressParams
): Promise<AdaSyncProgressResponse> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/sync/progress',
    port,
    ca,
  })
);
