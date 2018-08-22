// @flow
import type { AdaWalletV0 } from './types';
import { request } from './lib/request';

export type ImportAdaBackupJSONParams = {
  ca: string,
  filePath: string,
};

export const importAdaBackupJSON = (
  { ca, filePath }: ImportAdaBackupJSONParams,
): Promise<AdaWalletV0> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port: 8090,
    ca,
  }, {}, filePath)
);
