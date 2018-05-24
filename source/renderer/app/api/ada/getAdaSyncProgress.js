// @flow
import type { AdaSyncProgressResponse } from './types';
import { request } from './lib/request';

export type GetAdaSyncProgressParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
};

export const getAdaSyncProgress = (
  { apiParams }: GetAdaSyncProgressParams
): Promise<AdaSyncProgressResponse> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/sync/progress',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  })
);
