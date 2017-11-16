// @flow
import type { ApiWallet } from 'daedalus-client-api';
import type { WalletInitData } from './types';
import { request } from './lib/request';

export type NewAdaWalletParams = {
  ca: string,
  password: ?string,
  walletInitData: WalletInitData
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
