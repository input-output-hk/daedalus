// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export type DeleteAdaWalletParams = {
  walletId: string,
};

export const deleteAdaWallet = (
  config: RequestConfig,
  { walletId }: DeleteAdaWalletParams
): Promise<[]> => (
  request(Object.assign({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/wallets/${walletId}`,
  }, config))
);
