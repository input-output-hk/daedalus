// @flow
import { request } from './lib/request';

export type DeleteAdaWalletParams = {
  ca: string,
  port: number,
  walletId: string,
};

export const deleteAdaWallet = (
  { ca, walletId }: DeleteAdaWalletParams
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/wallets/${walletId}`,
    port,
    ca,
  })
);
