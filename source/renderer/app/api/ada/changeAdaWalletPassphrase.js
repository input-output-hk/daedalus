// @flow
import type { AdaWallet, RequestConfig } from './types';
import { request } from './lib/request';
import { encryptPassphrase } from './lib/encryptPassphrase';

export type ChangeAdaWalletPassphraseParams = {
  walletId: string,
  oldPassword: ?string,
  newPassword: ?string,
};

export const changeAdaWalletPassphrase = (
  config: RequestConfig,
  { walletId, oldPassword, newPassword }: ChangeAdaWalletPassphraseParams
): Promise<AdaWallet> => {
  const encryptedOldPassphrase = oldPassword ? encryptPassphrase(oldPassword) : null;
  const encryptedNewPassphrase = newPassword ? encryptPassphrase(newPassword) : null;
  return request(Object.assign({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/wallets/password/${walletId}`,
  }, config), { old: encryptedOldPassphrase, new: encryptedNewPassphrase });
};
