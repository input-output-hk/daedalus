// @flow
import type { AdaAccountV1 } from './types';
import { request } from './lib/v1/request';

export type GetAdaWalletAccountsParams = {
  ca: string,
  walletId: string,
};

export const getAdaWalletAccountsV1 = (
  { ca, walletId }: GetAdaWalletAccountsParams
): Promise<AdaAccountV1> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/v1/wallets/${walletId}/accounts`,
    port: 8090,
    ca,
  })
);
