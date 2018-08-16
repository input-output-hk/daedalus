// @flow
import type { AdaSyncProgressResponse } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

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
    port: environment.WALLET_PORT,
    ca,
  })
);
