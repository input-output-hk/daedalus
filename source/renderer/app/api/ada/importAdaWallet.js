// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type ImportAdaWalletParams = {
  filePath: string,
  walletPassword: ?string,
};

export const importAdaWallet = (
  { walletPassword, filePath }: ImportAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/keys',
  }, { passphrase: walletPassword }, filePath)
);
