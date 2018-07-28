// @flow
import type { AdaWallet, AdaWalletInitData } from './types';
import { request } from './lib/v1/request';

export type RestoreAdaWalletParams = {
  ca: string,
  walletInitData: AdaWalletInitData
};

export const restoreAdaWallet = (
  { ca, walletInitData }: RestoreAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/wallets',
    port: 8090,
    ca,
  }, {}, walletInitData)
);

