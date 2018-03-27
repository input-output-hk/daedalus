// @flow
import { request } from './lib/request';

export type AdaTestResetParams = {
  ca: string,
};

export const adaTestReset = (
  { ca }: AdaTestResetParams
): Promise<void> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/test/reset',
    port: 8090,
    ca,
  })
);
