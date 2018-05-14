// @flow
import type { AdaV1Wallets } from './types';
import { request } from './lib/v1/request';

export type GetAdaWalletParams = {
  ca: string,
  port: number,
};

export const getAdaWallets = (
  { ca, port }: GetAdaWalletParams
): Promise<AdaV1Wallets> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/wallets',
    port,
    ca,
  })
);
