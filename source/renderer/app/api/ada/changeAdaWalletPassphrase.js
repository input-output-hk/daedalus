// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';
import { encryptPassphrase } from './lib/encryptPassphrase';

export type ChangeAdaWalletPassphraseParams = {
  ca: string,
  port: number,
  walletId: string,
  oldPassword: ?string,
  newPassword: ?string,
};

export const changeAdaWalletPassphrase = (
  { ca, port, walletId, oldPassword, newPassword }: ChangeAdaWalletPassphraseParams
): Promise<AdaWallet> => {
  const encryptedOldPassphrase = oldPassword ? encryptPassphrase(oldPassword) : null;
  const encryptedNewPassphrase = newPassword ? encryptPassphrase(newPassword) : null;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/wallets/password/${walletId}`,
    port,
    ca,
  }, { old: encryptedOldPassphrase, new: encryptedNewPassphrase });
};
