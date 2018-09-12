// @flow
import { request } from '../../utils/requestV0';
import type { RequestConfig } from './types';

export type ExportWalletAsJSONParams = {
  walletId: string,
  filePath: string,
};

export const exportWalletAsJSON = (
  config: RequestConfig,
  { walletId, filePath }: ExportWalletAsJSONParams,
): Promise<[]> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
    ...config,
  }, {}, filePath)
);
