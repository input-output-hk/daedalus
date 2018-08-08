// @flow
import { request } from './lib/v1/request';

export type DeleteAdaWalletParams = {
  ca: string,
  walletId: string,
};

export const deleteAdaWallet = (
  { ca, walletId }: DeleteAdaWalletParams
): Promise<*> => (
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/v1/wallets/${walletId}`,
    port: 8090,
    ca,
  })
);
