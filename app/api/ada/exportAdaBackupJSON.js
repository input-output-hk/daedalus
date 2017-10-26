// @flow
import { request } from './lib/request';

export type ExportAdaBackupJSONPathParams = {
  walletId: string,
};

export type ExportAdaBackupJSONRawBodyParams = {
  filePath: string,
};

export const exportAdaBackupJSON = (
  ca: string,
  pathParams: ExportAdaBackupJSONPathParams,
  queryParams: {},
  rawBodyParams: ExportAdaBackupJSONRawBodyParams,
): Promise<[]> => {
  const { walletId } = pathParams;
  const { filePath } = rawBodyParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
    port: 8090,
    ca,
  }, queryParams, filePath);
};
