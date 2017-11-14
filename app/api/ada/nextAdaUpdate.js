// @flow
import { request } from './lib/request';

export type NextAdaUpdateParams = {
  ca: string,
};

export const nextAdaUpdate = (
  { ca }: NextAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/update',
    port: 8090,
    ca,
  })
);
