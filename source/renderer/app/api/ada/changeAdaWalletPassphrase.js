// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';
import { encryptPassphrase } from './lib/encryptPassphrase';

export type ChangeAdaWalletPassphraseParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  walletId: string,
  oldPassword: ?string,
  newPassword: ?string,
};

export const changeAdaWalletPassphrase = (
  { apiParams, walletId, oldPassword, newPassword }: ChangeAdaWalletPassphraseParams
): Promise<AdaWallet> => {
  const encryptedOldPassphrase = oldPassword ? encryptPassphrase(oldPassword) : null;
  const encryptedNewPassphrase = newPassword ? encryptPassphrase(newPassword) : null;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/wallets/password/${walletId}`,
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { old: encryptedOldPassphrase, new: encryptedNewPassphrase });
};
