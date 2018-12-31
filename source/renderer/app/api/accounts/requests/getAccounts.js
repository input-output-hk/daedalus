// @flow
import type { RequestConfig } from '../../common/types';
import type { Accounts } from '../types';
import { request } from '../../utils/request';

export type GetAccountsParams = {
  walletId: string,
};

export const getAccounts = (
  config: RequestConfig,
  { walletId }: GetAccountsParams
): Promise<Accounts> => (
  request({
    method: 'GET',
    path: `/api/v1/wallets/${walletId}/accounts`,
    ...config,
  })
);
