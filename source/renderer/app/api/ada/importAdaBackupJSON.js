// @flow
import type { AdaWalletV0 } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

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
    port: environment.WALLET_PORT,
    ca,
  }, {}, filePath)
);
