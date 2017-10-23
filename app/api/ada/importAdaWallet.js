// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type ImportAdaWalletQueryParams = {
  passphrase: ?string,
};

export const importAdaWallet = (
  ca: string, pathParams: {}, queryParams: ImportAdaWalletQueryParams
): Promise<ApiWallet> => {
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/keys',
    port: 8090,
    ca,
  }, queryParams);
};
