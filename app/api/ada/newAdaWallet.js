// @flow
import type { ApiWallet, ApiWalletInitData } from './types';
import { request } from './lib/request';

export type NewAdaWalletParams = {
  ca: string,
  password: ?string,
  walletInitData: ApiWalletInitData
};

export const newAdaWallet = (
  { ca, password, walletInitData }: NewAdaWalletParams
  ): Promise<ApiWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/new',
    port: 8090,
    ca,
  }, { passphrase: password }, walletInitData)
);
