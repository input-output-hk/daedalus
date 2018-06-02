// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/request';

export type RestoreAdaWalletParams = {
  walletPassword: ?string,
  walletInitData: AdaWalletInitData
};

export const restoreAdaWallet = (
  { walletPassword, walletInitData }: RestoreAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/restore',
  }, { passphrase: walletPassword }, walletInitData)
);

