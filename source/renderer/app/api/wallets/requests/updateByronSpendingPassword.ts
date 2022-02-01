import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const updateByronSpendingPassword = (
  config: RequestConfig,
  {
    walletId,
    oldPassword,
    newPassword,
  }: {
    walletId: string;
    oldPassword?: string;
    newPassword: string;
  }
): Promise<AdaWallet> =>
  request(
    {
      method: 'PUT',
      path: `/v2/byron-wallets/${getRawWalletId(walletId)}/passphrase`,
      ...config,
    },
    {},
    {
      old_passphrase: oldPassword,
      new_passphrase: newPassword,
    }
  );
