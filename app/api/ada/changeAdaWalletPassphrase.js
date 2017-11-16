// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';
import { encryptPassphrase } from './lib/encryptPassphrase';

export type ChangeAdaWalletPassphraseParams = {
  ca: string,
  walletId: string,
  oldPassword: ?string,
  newPassword: ?string,
};

export const changeAdaWalletPassphrase = (
  { ca, walletId, oldPassword, newPassword }: ChangeAdaWalletPassphraseParams
): Promise<ApiWallet> => {
  const encryptedOldPassphrase = oldPassword ? encryptPassphrase(oldPassword) : null;
  const encryptedNewPassphrase = newPassword ? encryptPassphrase(newPassword) : null;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/wallets/password/${walletId}`,
    port: 8090,
    ca,
  }, { old: encryptedOldPassphrase, new: encryptedNewPassphrase });
};
