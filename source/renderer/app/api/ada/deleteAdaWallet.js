// @flow
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type DeleteAdaWalletParams = {
  ca: string,
  walletId: string,
};

export const deleteAdaWallet = (
  { ca, walletId }: DeleteAdaWalletParams
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/wallets/${walletId}`,
    port: environment.WALLET_PORT,
    ca,
  })
);
