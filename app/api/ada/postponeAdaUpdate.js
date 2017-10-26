// @flow
import { request } from './lib/request';

export const postponeAdaUpdate = (ca: string): Promise<any> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/update/postpone',
    port: 8090,
    ca,
  })
);
