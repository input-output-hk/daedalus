// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';
import { encryptPassphrase } from './lib/encryptPassphrase';

export type ChangeAdaWalletPassphrasePathParams = {
  walletId: string,
};

export type ChangeAdaWalletPassphraseQueryParams = {
  old: ?string,
  new: ?string,
};

export const changeAdaWalletPassphrase = (
  ca: string,
  pathParams: ChangeAdaWalletPassphrasePathParams,
  queryParams: ChangeAdaWalletPassphraseQueryParams,
): Promise<ApiWallet> => {
  const { walletId } = pathParams;
  const encryptedOldPassphrase = queryParams.old ? encryptPassphrase(queryParams.old) : null;
  const encryptedNewPassphrase = queryParams.new ? encryptPassphrase(queryParams.new) : null;

  const newQueryParams = {
    old: encryptedOldPassphrase,
    new: encryptedNewPassphrase,
  };

  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/wallets/password/${walletId}`,
    port: 8090,
    ca,
  }, newQueryParams);
};
