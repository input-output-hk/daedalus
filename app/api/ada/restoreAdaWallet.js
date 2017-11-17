// @flow
import type { ApiWallet } from 'daedalus-client-api';
import type { WalletInitData } from './types';
import { request } from './lib/request';

export type RestoreAdaWalletParams = {
  ca: string,
  walletPassword: ?string,
  walletInitData: WalletInitData
};

export const restoreAdaWallet = (
  { ca, walletPassword, walletInitData }: RestoreAdaWalletParams
): Promise<ApiWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/restore',
    port: 8090,
    ca,
  }, { passphrase: walletPassword }, walletInitData)
);

