// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type ImportAdaBackupJSONParams = {
  ca: string,
  port: number,
  filePath: string,
};

export const importAdaBackupJSON = (
  { ca, filePath }: ImportAdaBackupJSONParams,
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port,
    ca,
  }, {}, filePath)
);
