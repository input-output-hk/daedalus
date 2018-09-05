// @flow
import { request } from './lib/v1/request';
import type { RequestConfig } from './types';

export type DeleteAdaWalletParams = {
  walletId: string,
};

export const deleteAdaWallet = (
  config: RequestConfig,
  { walletId }: DeleteAdaWalletParams
): Promise<*> => (
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/v1/wallets/${walletId}`,
    ...config,
  })
);
