// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type ImportAdaBackupJSONRawBodyParams = {
  filePath: string,
};

export const importAdaBackupJSON = (
  ca: string, pathParams: {}, queryParams: {}, rawBodyParams: ImportAdaBackupJSONRawBodyParams,
): Promise<ApiWallet> => {
  const { filePath } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port: 8090,
    ca,
  }, queryParams, filePath);
};
