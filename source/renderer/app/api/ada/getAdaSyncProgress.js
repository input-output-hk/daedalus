// @flow
import type { AdaSyncProgressResponse } from './types';
import { request } from './lib/request';

export type GetAdaSyncProgressParams = {
};

export const getAdaSyncProgress = (
): Promise<AdaSyncProgressResponse> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/sync/progress',
  })
);
