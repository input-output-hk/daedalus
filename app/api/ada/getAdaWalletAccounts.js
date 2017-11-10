// @flow
import type { ApiAccounts } from 'daedalus-client-api';
import { request } from './lib/request';

export type getAdaWalletAccountsQueryParams = {
  walletId: ?string,
};

export const getAdaWalletAccounts = (
  ca: string, pathParams: {}, queryParams: getAdaWalletAccountsQueryParams
): Promise<ApiAccounts> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/accounts',
    port: 8090,
    ca,
  }, queryParams)
);
