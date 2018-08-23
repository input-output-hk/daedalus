// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/v1/request';

export type NewAdaWalletParams = {
  ca: string,
  walletInitData: AdaWalletInitData
};

export const newAdaWallet = (
  { ca, walletInitData }: NewAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/wallets',
    port: 8090,
    ca,
  }, {}, walletInitData)
);
