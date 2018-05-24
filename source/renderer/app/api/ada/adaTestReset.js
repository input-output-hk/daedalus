// @flow
import { request } from './lib/request';

export type AdaTestResetParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
};

export const adaTestReset = (
  { apiParams }: AdaTestResetParams
): Promise<void> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/test/reset',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  })
);
