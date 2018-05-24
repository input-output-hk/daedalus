// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/request';

export type NewAdaWalletParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  password: ?string,
  walletInitData: AdaWalletInitData
};

export const newAdaWallet = (
  { apiParams, password, walletInitData }: NewAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/new',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { passphrase: password }, walletInitData)
);
