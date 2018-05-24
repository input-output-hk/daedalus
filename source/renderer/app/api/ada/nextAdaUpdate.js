// @flow
import { request } from './lib/request';

export type NextAdaUpdateParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
};

export const nextAdaUpdate = (
  { apiParams }: NextAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/update',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  })
);
