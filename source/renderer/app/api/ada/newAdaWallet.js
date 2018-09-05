// @flow
import type { AdaWallet, AdaWalletInitData, RequestConfig } from './types';
import { request } from './lib/v1/request';

export type NewAdaWalletParams = {
  walletInitData: AdaWalletInitData
};

export const newAdaWallet = (
  config: RequestConfig,
  { walletInitData }: NewAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/wallets',
    ...config,
  }, {}, walletInitData)
);
