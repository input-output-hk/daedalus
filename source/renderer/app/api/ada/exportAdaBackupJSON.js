// @flow
import { request } from './lib/request';

export type ExportAdaBackupJSONParams = {
  walletId: string,
  filePath: string,
};

export const exportAdaBackupJSON = (
  { walletId, filePath }: ExportAdaBackupJSONParams,
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
  }, {}, filePath)
);
