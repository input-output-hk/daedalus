// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/request';

export type RestoreAdaWalletParams = {
  ca: string,
  port: number,
  walletPassword: ?string,
  walletInitData: AdaWalletInitData
};

export const restoreAdaWallet = (
  { ca, port, walletPassword, walletInitData }: RestoreAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/restore',
    port,
    ca,
  }, { passphrase: walletPassword }, walletInitData)
);

