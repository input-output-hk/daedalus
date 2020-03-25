// @flow
import type { RequestConfig } from '../../common/types';
import type { ByronWalletAddress } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export type CreateAddressParams = {
  spendingPassword?: string,
  accountIndex: number,
  walletId: string,
};

export const createByronWalletAddress = (
  config: RequestConfig,
  { spendingPassword, accountIndex, walletId }: CreateAddressParams
): Promise<ByronWalletAddress> =>
  request(
    {
      method: 'POST',
      path: '/v2/byron-wallets/addresses',
      ...config,
    },
    {},
    {
      spendingPassword,
      accountIndex,
      walletId: getRawWalletId(walletId),
    }
  );
