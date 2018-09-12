// @flow
import type { AdaWallet, RequestConfig } from './types';
import { request } from '../../utils/requestV0';

export const importWalletAsJSON = (
  config: RequestConfig,
  filePath: string,
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    ...config,
  }, {}, filePath)
);
