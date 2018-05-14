// @flow
import { request } from './lib/request';

export type NextAdaUpdateParams = {
  ca: string,
  port: number,
};

export const nextAdaUpdate = (
  { ca, port }: NextAdaUpdateParams
): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/update',
    port,
    ca,
  })
);
