// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/request';

export type NewAdaWalletParams = {
  ca: string,
  port: number,
  password: ?string,
  walletInitData: AdaWalletInitData
};

export const newAdaWallet = (
  { ca, port, password, walletInitData }: NewAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/new',
    port,
    ca,
  }, { passphrase: password }, walletInitData)
);
