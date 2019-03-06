// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

export type DeleteWalletParams = {
  walletId: string,
};

export const deleteWallet = (
  config: RequestConfig,
  { walletId }: DeleteWalletParams
): Promise<*> =>
  request({
    method: 'DELETE',
    path: `/api/v1/wallets/${walletId}`,
    ...config,
  });
