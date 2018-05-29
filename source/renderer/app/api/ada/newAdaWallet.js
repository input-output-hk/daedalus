// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/request';

export type NewAdaWalletParams = {
  password: ?string,
  walletInitData: AdaWalletInitData
};

export const newAdaWallet = (
  { password, walletInitData }: NewAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/new',
  }, { passphrase: password }, walletInitData)
);
