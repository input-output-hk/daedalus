// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type getAdaWalletAccountsQueryParams = {
  accountId: ?string,
};

export const getAdaWalletAccounts = (
  ca: string, pathParams: {}, queryParams: getAdaWalletAccountsQueryParams
): Promise<ApiWallet> => {
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/accounts/${accountId}`,
    port: 8090,
    ca,
  }, queryParams);
};
