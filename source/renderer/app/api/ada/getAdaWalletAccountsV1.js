// @flow
import type { AdaAccountsV1 } from './types';
import { request } from './lib/request';


export type GetAdaWalletAccountsParams = {
  ca: string,
  walletId: string,
};

export const getAdaWalletAccountsV1 = (
  { ca, walletId }: GetAdaWalletAccountsParams
): Promise<AdaAccountsV1> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/v1/wallets/${walletId}/accounts`,
    port: 8090,
    ca,
  })
);
