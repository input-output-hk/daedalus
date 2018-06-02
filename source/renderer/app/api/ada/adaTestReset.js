// @flow
import { request } from './lib/request';

export type AdaTestResetParams = {
};

export const adaTestReset = (
): Promise<void> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/test/reset',
  })
);
