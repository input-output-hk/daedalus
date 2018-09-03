// @flow
import type { AdaWallet, RequestConfig } from './types';
import { request } from './lib/v1/request';

export type ImportAdaWalletParams = {
  filePath: string,
  spendingPassword: ?string,
};

export const importAdaWallet = (
  config: RequestConfig,
  walletImportData: ImportAdaWalletParams
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/internal/import-wallet',
    ...config,
  }, {}, walletImportData)
);
