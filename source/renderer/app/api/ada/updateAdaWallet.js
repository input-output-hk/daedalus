// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type UpdateAdaWalletParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  walletId: string,
  walletMeta: {
    cwName: string,
    cwAssurance: string,
    cwUnit: number,
  }
};

export const updateAdaWallet = (
  { apiParams, walletId, walletMeta }: UpdateAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'PUT',
    path: `/api/wallets/${walletId}`,
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, {}, walletMeta)
);
