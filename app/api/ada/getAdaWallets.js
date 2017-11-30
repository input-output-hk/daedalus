// @flow
import type { AdaWallets } from './types';
import { request } from './lib/request';

export type GetAdaWalletParams = {
  ca: string,
};

export const getAdaWallets = (
  { ca }: GetAdaWalletParams
  ): Promise<AdaWallets> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/wallets',
    port: 8090,
    ca,
  })
);
