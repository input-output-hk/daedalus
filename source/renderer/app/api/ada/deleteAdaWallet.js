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
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/wallets/${walletId}`,
    port: config.port,
    ca: config.ca,
  })
);
