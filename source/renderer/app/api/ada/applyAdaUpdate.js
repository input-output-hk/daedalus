// @flow
import { request } from './lib/request';

export type ApplyAdaUpdateParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
};

export const applyAdaUpdate = (
  { apiParams }: ApplyAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/apply',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  })
);
