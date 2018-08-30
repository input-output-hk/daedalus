// @flow
import type { AdaWallet, AdaWalletInitData, RequestConfig } from './types';
import { request } from './lib/request';

export type NewAdaWalletParams = {
  password: ?string,
  walletInitData: AdaWalletInitData
};

export const newAdaWallet = (
  config: RequestConfig,
  { password, walletInitData }: NewAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/new',
    port: config.port,
    ca: config.ca,
  }, { passphrase: password }, walletInitData)
);
