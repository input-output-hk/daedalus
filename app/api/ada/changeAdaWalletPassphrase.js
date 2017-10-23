// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type changeAdaWalletPassphrasePathParams = {
  walletId: string,
};

export type changeAdaWalletPassphraseQueryParams = {
  old: ?string,
  new: ?string,
};

export const changeAdaWalletPassphrase = (
  ca: string, pathParams: changeAdaWalletPassphrasePathParams, queryParams: changeAdaWalletPassphraseQueryParams
): Promise<ApiWallet> => {
  const { walletId } = pathParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/wallets/password/${walletId}`,
    port: 8090,
    ca,
  }, queryParams);
};
