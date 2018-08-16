// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

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
    port: environment.WALLET_PORT,
    ca,
  }, { passphrase: password }, walletInitData)
);
