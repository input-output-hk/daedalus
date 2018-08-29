// @flow
import { request } from './lib/request';
import type { RequestConfig } from './types';

export type ExportAdaBackupJSONParams = {
  walletId: string,
  filePath: string,
};

export const exportAdaBackupJSON = (
  config: RequestConfig,
  { walletId, filePath }: ExportAdaBackupJSONParams,
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
    port: config.port,
    ca: config.ca,
  }, {}, filePath)
);
