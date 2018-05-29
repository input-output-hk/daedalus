// @flow
import type { AdaAccounts } from './types';
import { request } from './lib/request';


export type GetAdaWalletAccountsParams = {
  walletId: string,
};

export const getAdaWalletAccounts = (
  { walletId }: GetAdaWalletAccountsParams
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
  }, { accountId: walletId })
);
