// @flow
import type { AdaAccounts } from './types';
import { request } from './lib/v1/request';

export type GetAdaWalletAccountsParams = {
  ca: string,
  walletId: string,
};

export const getAdaWalletAccounts = (
  { ca, walletId }: GetAdaWalletAccountsParams
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/v1/wallets/${walletId}/accounts`,
    port: 8090,
    ca,
  })
);
