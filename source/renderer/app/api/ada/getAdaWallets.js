// @flow
import type { AdaV1Wallets } from './types';
import { request } from './lib/v1/request';

export type GetAdaWalletParams = {
};

export const getAdaWallets = (
): Promise<AdaV1Wallets> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/wallets',
  })
);
