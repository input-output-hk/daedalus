// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type restoreAdaWalletQueryParams = {
  passphrase: ?string,
};

export const deleteAdaWallet = (
  ca: string, pathParams: {}, queryParams: restoreAdaWalletQueryParams
): Promise<ApiWallet> => {
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/restore',
    port: 8090,
    ca,
  }, queryParams);
};
