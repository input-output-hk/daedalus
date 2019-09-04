// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { encryptPassphrase } from '../../utils';
import { request } from '../../utils/request';

export type ChangeSpendingPasswordParams = {
  walletId: string,
  oldPassword: ?string,
  newPassword: ?string,
};

export const changeSpendingPassword = (
  config: RequestConfig,
  { walletId, oldPassword, newPassword }: ChangeSpendingPasswordParams
): Promise<AdaWallet> => {
  const encryptedOldPassphrase = oldPassword
    ? encryptPassphrase(oldPassword)
    : '';
  const encryptedNewPassphrase = newPassword
    ? encryptPassphrase(newPassword)
    : '';
  return request(
    {
      method: 'PUT',
      path: `/v2/wallets/${walletId}/passphrase`,
      ...config,
    },
    {},
    {
      old_passphrase: encryptedOldPassphrase,
      new_passphrase: encryptedNewPassphrase,
    }
  );
};
