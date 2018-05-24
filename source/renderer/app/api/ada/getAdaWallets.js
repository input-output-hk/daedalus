// @flow
import type { AdaV1Wallets } from './types';
import { request } from './lib/v1/request';

export type GetAdaWalletParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
};

export const getAdaWallets = (
  { apiParams }: GetAdaWalletParams
): Promise<AdaV1Wallets> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/wallets',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  })
);
