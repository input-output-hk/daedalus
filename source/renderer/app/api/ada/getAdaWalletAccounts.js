// @flow
import type { AdaAccounts, RequestConfig } from './types';
import { request } from './lib/v1/request';

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
    path: `/api/v1/wallets/${walletId}/accounts`,
    ...config,
  })
);
