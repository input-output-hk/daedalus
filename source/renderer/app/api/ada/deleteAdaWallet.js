// @flow
import { request } from './lib/request';

export type DeleteAdaWalletParams = {
  walletId: string,
};

export const deleteAdaWallet = (
  { walletId }: DeleteAdaWalletParams
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/wallets/${walletId}`,
  })
);
