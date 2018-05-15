// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type ImportAdaBackupJSONParams = {
  ca: string,
  port: number,
  filePath: string,
};

export const importAdaBackupJSON = (
  { ca, port, filePath }: ImportAdaBackupJSONParams,
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port,
    ca,
  }, {}, filePath)
);
