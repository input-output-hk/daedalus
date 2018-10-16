// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type RestoreAdaWalletParams = {
  ca: string,
  walletPassword: ?string,
  walletInitData: AdaWalletInitData
};

export const restoreAdaWallet = (
  { ca, walletPassword, walletInitData }: RestoreAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/restore',
    port: environment.WALLET_PORT,
    ca,
  }, { passphrase: walletPassword }, walletInitData)
);

