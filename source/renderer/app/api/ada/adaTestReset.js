// @flow
import { request } from './lib/request';

export type AdaTestResetParams = {
  ca: string,
  port: number,
};

export const adaTestReset = (
  { ca, port }: AdaTestResetParams
): Promise<void> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/test/reset',
    port,
    ca,
  })
);
