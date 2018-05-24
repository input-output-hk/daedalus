// @flow
import type { AdaLocalTimeDifference } from './types';
import { request } from './lib/request';

export type GetAdaLocalTimeDifferenceParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
};

export const getAdaLocalTimeDifference = (
  { apiParams }: GetAdaLocalTimeDifferenceParams
): Promise<AdaLocalTimeDifference> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/time/difference',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  })
);
