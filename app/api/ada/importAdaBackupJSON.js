// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type ImportAdaBackupJSONParams = {
  ca: string,
  filePath: string,
};

export const importAdaBackupJSON = (
  { ca, filePath }: ImportAdaBackupJSONParams,
): Promise<ApiWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port: 8090,
    ca,
  }, {}, filePath)
);
