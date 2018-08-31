// @flow
import type { AdaWalletV0, RequestConfig } from './types';
import { request } from './lib/request';

export type ImportAdaWalletParams = {
  filePath: string,
  walletPassword: ?string,
};

export const importAdaWallet = (
  config: RequestConfig,
  { walletPassword, filePath }: ImportAdaWalletParams
): Promise<AdaWalletV0> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/keys',
    ...config,
  }, { passphrase: walletPassword }, filePath)
);
