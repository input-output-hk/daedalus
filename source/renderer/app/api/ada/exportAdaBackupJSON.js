// @flow
import { request } from './lib/request';

export type ExportAdaBackupJSONParams = {
  ca: string,
  port: number,
  walletId: string,
  filePath: string,
};

export const exportAdaBackupJSON = (
  { ca, port, walletId, filePath }: ExportAdaBackupJSONParams,
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
    port,
    ca,
  }, {}, filePath)
);
