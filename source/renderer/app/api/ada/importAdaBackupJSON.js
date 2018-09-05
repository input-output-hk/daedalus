// @flow
import type { AdaWallet, RequestConfig } from './types';
import { request } from './lib/v1/request';

export const importAdaBackupJSON = (
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
