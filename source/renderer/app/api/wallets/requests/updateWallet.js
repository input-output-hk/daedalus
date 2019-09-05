// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export type UpdateWalletParams = {
  walletId: string,
  name: string,
};

export const updateWallet = (
  config: RequestConfig,
  { walletId, name }: UpdateWalletParams
): Promise<AdaWallet> =>
  request(
    {
      method: 'PUT',
      path: `/api/v1/wallets/${walletId}`,
      ...config,
    },
    {},
    { name }
  );
