// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/request';

export type NewAdaWalletParams = {
  ca: string,
  password: ?string,
  walletInitData: AdaWalletInitData
};

export const newAdaWallet = (
  { ca, password, walletInitData }: NewAdaWalletParams
  ): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/new',
    port: 8090,
    ca,
  }, { passphrase: password }, walletInitData)
);
