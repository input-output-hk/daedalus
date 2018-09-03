// @flow
import type { AdaWallet } from './types';
import { request } from './lib/v1/request';
import environment from '../../../../common/environment';

export type ImportAdaWalletParams = {
  ca: string,
  walletImportData: {},
};

export const importAdaWallet = (
  { ca, walletImportData }: ImportAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/internal/import-wallet',
    port: environment.WALLET_PORT,
    ca,
  }, {}, walletImportData)
);
