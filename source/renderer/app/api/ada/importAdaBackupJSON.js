// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type ImportAdaBackupJSONParams = {
  filePath: string,
};

export const importAdaBackupJSON = (
  { filePath }: ImportAdaBackupJSONParams,
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
  }, {}, filePath)
);
