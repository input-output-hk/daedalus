// @flow
import type { AdaWallet, AdaWalletInitData, RequestConfig } from './types';
import { request } from '../../utils/request';

export type CreateWalletParams = {
  walletInitData: AdaWalletInitData
};

export const createWallet = (
  config: RequestConfig,
  { walletInitData }: CreateWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/wallets',
    ...config,
  }, {}, walletInitData)
);
