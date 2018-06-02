// @flow
import type { AdaAccounts } from './types';
import { request } from './lib/request';


export type GetAdaWalletAccountsParams = {
  ca: string,
  port: number,
  walletId: string,
};

export const getAdaWalletAccounts = (
  { ca, port, walletId }: GetAdaWalletAccountsParams
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    port,
    ca,
  }, { accountId: walletId })
);
