// @flow
import { request } from './lib/request';

export type DeleteAdaWalletParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  walletId: string,
};

export const deleteAdaWallet = (
  { apiParams, walletId }: DeleteAdaWalletParams
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'DELETE',
    path: `/api/wallets/${walletId}`,
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  })
);
