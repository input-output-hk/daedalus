// @flow
import type { AdaAccounts, RequestConfig } from './types';
import { request } from '../../utils/request';

export type GetAccountsParams = {
  walletId: string,
};

export const getAccounts = (
  config: RequestConfig,
  { walletId }: GetAccountsParams
): Promise<AdaAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/v1/wallets/${walletId}/accounts`,
    ...config,
  })
);
