// @flow
import { request } from './lib/request';

export const getLocalTimeDifference = (ca: string): Promise<number> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/time/difference',
    port: 8090,
    ca,
  })
);
