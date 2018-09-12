// @flow
import type { AdaWallet, RequestConfig } from './types';
import { encryptPassphrase } from '../../utils';
import { request } from '../../utils/request';

export type ChangeSpendingPasswordParams = {
  walletId: string,
  oldPassword?: string,
  newPassword: string,
};

export const changeSpendingPassword = (
  config: RequestConfig,
  { walletId, oldPassword, newPassword }: ChangeSpendingPasswordParams
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
