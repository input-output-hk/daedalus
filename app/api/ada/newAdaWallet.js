// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type newAdaWalletQueryParams = {
  passphrase: ?string,
  CWalletInit: ?string,
};

export const newAdaWallet = (
  ca: string, pathParams: {}, queryParams: newAdaWalletQueryParams
): Promise<ApiWallet> => {
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/new',
    port: 8090,
    ca,
  }, queryParams);
};
