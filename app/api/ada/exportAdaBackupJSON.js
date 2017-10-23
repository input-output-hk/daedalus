// @flow
import type { ApiWallet } from 'daedalus-client-api';
import { request } from './lib/request';

export type exportAdaBackupJSONPathParams = {
  walletId: string,
};

export const exportAdaBackupJSON = (
  ca: string, pathParams: exportAdaBackupJSONPathParams
): Promise<ApiWallet> => {
  const { walletId } = pathParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
    port: 8090,
    ca,
  });
};
