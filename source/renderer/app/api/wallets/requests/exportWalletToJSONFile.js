// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/requestV0';

export type ExportWalletToJSONFileParams = {
  walletId: string,
  filePath: string,
};

export const exportWalletToJSONFile = (
  config: RequestConfig,
  { walletId, filePath }: ExportWalletToJSONFileParams,
): Promise<[]> => (
  request({
    method: 'POST',
    path: `/api/backup/export/${walletId}`,
    ...config,
  }, {}, filePath)
);
