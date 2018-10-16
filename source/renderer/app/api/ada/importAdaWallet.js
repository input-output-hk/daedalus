// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type ImportAdaWalletParams = {
  ca: string,
  filePath: string,
  walletPassword: ?string,
};

export const importAdaWallet = (
  { ca, walletPassword, filePath }: ImportAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/wallets/keys',
    port: environment.WALLET_PORT,
    ca,
  }, { passphrase: walletPassword }, filePath)
);
