// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export const importAdaBackupJSON = (
  ca: string, pathParams: {}
): Promise<ApiWallet> => {
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port: 8090,
    ca,
  });
};
