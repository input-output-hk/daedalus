// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export type ChangeSpendingPasswordParams = {
  walletId: string,
  oldPassword: string,
  newPassword: string,
};

export const changeSpendingPassword = (
  config: RequestConfig,
  { walletId, oldPassword, newPassword }: ChangeSpendingPasswordParams
): Promise<AdaWallet> =>
  request(
    {
      method: 'PUT',
      path: `/v2/wallets/${walletId}/passphrase`,
      ...config,
    },
    {},
    {
      old_passphrase: oldPassword,
      new_passphrase: newPassword,
    }
  );
