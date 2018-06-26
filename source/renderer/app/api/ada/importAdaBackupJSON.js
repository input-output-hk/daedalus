// @flow
import type { AdaWallet } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type ImportAdaBackupJSONParams = {
  ca: string,
  filePath: string,
};

export const importAdaBackupJSON = (
  { ca, filePath }: ImportAdaBackupJSONParams,
): Promise<AdaWallet> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/backup/import',
    port: environment.WALLET_PORT,
    ca,
  }, {}, filePath)
);
