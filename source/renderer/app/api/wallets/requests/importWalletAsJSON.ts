import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/requestV0';

export const importWalletAsJSON = (
  config: RequestConfig,
  filePath: string
): Promise<AdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/api/backup/import',
      ...config,
    },
    {},
    filePath
  );
