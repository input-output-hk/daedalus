// @flow
import { request } from './lib/request';

export const applyAdaUpdate = (ca: string): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/apply',
    port: 8090,
    ca,
  })
);
