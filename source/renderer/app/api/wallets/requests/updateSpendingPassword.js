// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet, UpdateSpendingPasswordRequest } from '../types';
import { request } from '../../utils/request';

export const updateSpendingPassword = (
  config: RequestConfig,
  { walletId, oldPassword, newPassword }: UpdateSpendingPasswordRequest
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
