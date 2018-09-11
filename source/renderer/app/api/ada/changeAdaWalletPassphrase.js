// @flow
import type { AdaWallet, RequestConfig } from './types';
import { request } from './lib/v1/request';
import { encryptPassphrase } from './lib/encryptPassphrase';

export type ChangeAdaWalletPassphraseParams = {
  walletId: string,
  oldPassword?: string,
  newPassword: string,
};

export const changeAdaWalletPassphrase = (
  config: RequestConfig,
  { walletId, oldPassword, newPassword }: ChangeAdaWalletPassphraseParams
): Promise<AdaWallet> => {
  const encryptedOldPassphrase = oldPassword ? encryptPassphrase(oldPassword) : '';
  const encryptedNewPassphrase = newPassword ? encryptPassphrase(newPassword) : '';
  return request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/v1/wallets/${walletId}/password`,
    ...config,
  }, {}, { old: encryptedOldPassphrase, new: encryptedNewPassphrase });
};
