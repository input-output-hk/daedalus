// @flow
import type { AdaV1Wallet } from './types';
import { request } from './lib/v1/request';
import { encryptPassphrase } from './lib/encryptPassphrase';

export type ChangeAdaWalletPassphraseParams = {
  ca: string,
  walletId: string,
  oldPassword?: string,
  newPassword: string,
};

export const changeAdaWalletPassphrase = (
  { ca, walletId, oldPassword, newPassword }: ChangeAdaWalletPassphraseParams
): Promise<AdaV1Wallet> => {
  const encryptedOldPassphrase = oldPassword ? encryptPassphrase(oldPassword) : '';
  const encryptedNewPassphrase = newPassword ? encryptPassphrase(newPassword) : '';
  return request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/v1/wallets/${walletId}/password`,
    port: 8090,
    ca,
  }, {}, { old: encryptedOldPassphrase, new: encryptedNewPassphrase });
};
