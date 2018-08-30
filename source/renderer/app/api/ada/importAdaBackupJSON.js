// @flow
import type { AdaWallet, RequestConfig } from './types';
import { request } from './lib/request';

export type ImportAdaBackupJSONParams = {
  filePath: string,
};

export const importAdaBackupJSON = (
  config: RequestConfig,
  { filePath }: ImportAdaBackupJSONParams,
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port: config.port,
    ca: config.ca,
  }, {}, filePath)
);
