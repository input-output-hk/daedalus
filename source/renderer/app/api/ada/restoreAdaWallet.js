// @flow
import type { AdaWallet, AdaWalletInitData, RequestConfig } from './types';
import { request } from './lib/v1/request';

export type RestoreAdaWalletParams = {
  walletInitData: AdaWalletInitData
};

export const restoreAdaWallet = (
  config: RequestConfig,
  { walletInitData }: RestoreAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/wallets',
    ...config,
  }, {}, walletInitData)
);
