// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type ImportAdaWalletParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  filePath: string,
  walletPassword: ?string,
};

export const importAdaWallet = (
  { apiParams, walletPassword, filePath }: ImportAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/keys',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { passphrase: walletPassword }, filePath)
);
