import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export type ImportWalletAsKey = {
  filePath: string;
  spendingPassword: string;
};
export const importWalletAsKey = (
  config: RequestConfig,
  walletImportData: ImportWalletAsKey
): Promise<AdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/api/internal/import-wallet',
      ...config,
    },
    {},
    walletImportData
  );
