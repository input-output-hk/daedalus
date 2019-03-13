// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export type ImportWalletFromKeyFile = {
  filePath: string,
  spendingPassword?: string,
};

export const importWalletFromKeyFile = (
  config: RequestConfig,
  walletImportData: ImportWalletFromKeyFile
): Promise<AdaWallet> => (
  request({
    method: 'POST',
    path: '/api/internal/import-wallet',
    ...config,
  }, {}, walletImportData)
);
