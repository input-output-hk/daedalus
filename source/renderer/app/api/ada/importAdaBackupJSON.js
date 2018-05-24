// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';

export type ImportAdaBackupJSONParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  filePath: string,
};

export const importAdaBackupJSON = (
  { apiParams, filePath }: ImportAdaBackupJSONParams,
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, {}, filePath)
);
