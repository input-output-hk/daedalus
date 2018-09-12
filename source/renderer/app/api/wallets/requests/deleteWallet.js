// @flow
import { request } from '../../utils/request';
import type { RequestConfig } from './types';

export type DeleteWalletParams = {
  walletId: string,
};

export const deleteWallet = (
  config: RequestConfig,
  { walletId }: DeleteWalletParams
): Promise<*> => (
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/v1/wallets/${walletId}`,
    ...config,
  })
);
