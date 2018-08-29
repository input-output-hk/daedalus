// @flow
import { request } from './lib/v1/request';

export type NextAdaUpdateParams = {
  ca: string,
};

export const nextAdaUpdate = (
  { ca }: NextAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/node-settings',
    port: 8090,
    ca,
  })
);
