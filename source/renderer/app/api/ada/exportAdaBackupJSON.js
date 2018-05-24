// @flow
import { request } from './lib/request';

export type ExportAdaBackupJSONParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  walletId: string,
  filePath: string,
};

export const exportAdaBackupJSON = (
  { apiParams, walletId, filePath }: ExportAdaBackupJSONParams,
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, {}, filePath)
);
