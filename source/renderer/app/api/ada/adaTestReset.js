// @flow
import { request } from './lib/request';
import environment from '../../../../common/environment';

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
    port: environment.WALLET_PORT,
    ca,
  })
);
