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
  request(Object.assign({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
  }, config), {}, filePath)
);
