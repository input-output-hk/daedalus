// @flow
import type { ApiWallet } from './types';
import { request } from './lib/request';

export type ImportAdaWalletParams = {
  ca: string,
  filePath: string,
  walletPassword: ?string,
};

export const importAdaWallet = (
  { ca, walletPassword, filePath }: ImportAdaWalletParams
): Promise<ApiWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/keys',
    port: 8090,
    ca,
  }, { passphrase: walletPassword }, filePath)
);
