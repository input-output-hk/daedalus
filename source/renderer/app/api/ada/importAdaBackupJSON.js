// @flow
import type { AdaWalletV0, RequestConfig } from './types';
import { request } from './lib/request';

export type ImportAdaBackupJSONParams = {
  filePath: string,
};

export const importAdaBackupJSON = (
  config: RequestConfig,
  { filePath }: ImportAdaBackupJSONParams
): Promise<AdaWalletV0> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    ...config,
  }, {}, filePath)
);
