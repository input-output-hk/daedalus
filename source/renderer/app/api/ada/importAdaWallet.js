// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type ImportAdaWalletParams = {
  ca: string,
  port: number,
  filePath: string,
  walletPassword: ?string,
};

export const importAdaWallet = (
  { ca, port, walletPassword, filePath }: ImportAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/keys',
    port,
    ca,
  }, { passphrase: walletPassword }, filePath)
);
