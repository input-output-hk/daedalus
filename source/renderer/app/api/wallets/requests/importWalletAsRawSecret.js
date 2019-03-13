// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export type ImportWalletAsRawSecret = {
  rawSecret: string,
  spendingPassword?: string,
};

export const importWalletAsRawSecret = (
  config: RequestConfig,
  walletImportData: ImportWalletAsRawSecret
): Promise<AdaWallet> => (
  request({
    method: 'POST',
    path: '/api/internal/import-wallet',
    ...config,
  }, {}, walletImportData)
);
