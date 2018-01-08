// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type ImportAdaBackupJSONParams = {
  ca: string,
  filePath: string,
};

export const importAdaBackupJSON = (
  { ca, filePath }: ImportAdaBackupJSONParams,
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port: 8090,
    ca,
  }, {}, filePath)
);
