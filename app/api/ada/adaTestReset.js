// @flow
import { request } from './lib/request';

export const adaTestReset = (ca: string): Promise<void> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/test/reset',
    port: 8090,
    ca,
  })
);
