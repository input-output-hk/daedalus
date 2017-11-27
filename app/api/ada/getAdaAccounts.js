// @flow
import type { ApiAccounts } from './types';
import { request } from './lib/request';

export type GetAdaAccountsParams = {
  ca: string,
  walletId: string,
  filePath: string,
};

export const getAdaAccounts = (
  { ca }: GetAdaAccountsParams
): Promise<ApiAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    port: 8090,
    ca,
  })
);
