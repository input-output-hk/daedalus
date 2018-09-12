// @flow
import type { AdaWallet, AdaWalletInitData, RequestConfig } from './types';
import { request } from '../../utils/request';

export type RestoreWalletParams = {
  walletInitData: AdaWalletInitData
};

export const restoreWallet = (
  config: RequestConfig,
  { walletInitData }: RestoreWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/wallets',
    ...config,
  }, {}, walletInitData)
);
