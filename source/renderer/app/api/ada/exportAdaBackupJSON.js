// @flow
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type ExportAdaBackupJSONParams = {
  ca: string,
  walletId: string,
  filePath: string,
};

export const exportAdaBackupJSON = (
  { ca, walletId, filePath }: ExportAdaBackupJSONParams,
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
    port: environment.WALLET_PORT,
    ca,
  }, {}, filePath)
);
