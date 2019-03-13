// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export type ImportWalletFromSecretKey = {
  rawSecret: string,
  spendingPassword?: string,
};

export const importWalletFromSecretKey = (
  config: RequestConfig,
  walletImportData: ImportWalletFromSecretKey
): Promise<AdaWallet> => (
  request({
    method: 'POST',
    path: '/api/internal/import-wallet',
    ...config,
  }, {}, walletImportData)
);
