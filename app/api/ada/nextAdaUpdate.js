// @flow
import { request } from './lib/request';

export const nextAdaUpdate = (ca: string): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/update',
    port: 8090,
    ca,
  })
);
