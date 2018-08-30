// @flow
import type { AdaWallet, AdaWalletInitData, RequestConfig } from './types';
import { request } from './lib/request';

export type RestoreAdaWalletParams = {
  walletPassword: ?string,
  walletInitData: AdaWalletInitData
};

export const restoreAdaWallet = (
  config: RequestConfig,
  { walletPassword, walletInitData }: RestoreAdaWalletParams
): Promise<AdaWallet> => (
  request(Object.assign({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/restore',
  }, config), { passphrase: walletPassword }, walletInitData)
);

