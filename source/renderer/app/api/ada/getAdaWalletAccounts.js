// @flow
import type { AdaAccounts, RequestConfig } from './types';
import { request } from './lib/request';

export type GetAdaWalletAccountsParams = {
  walletId: string,
};

export const getAdaWalletAccounts = (
  config: RequestConfig,
  { walletId }: GetAdaWalletAccountsParams
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    ...config
  }, { accountId: walletId })
);
