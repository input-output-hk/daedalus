// @flow
import type { AdaWallet, RequestConfig } from './types';
import { request } from './lib/request';

export type ImportAdaWalletParams = {
  filePath: string,
  walletPassword: ?string,
};

export const importAdaWallet = (
  config: RequestConfig,
  { walletPassword, filePath }: ImportAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/keys',
    ...config
  }, { passphrase: walletPassword }, filePath)
);
