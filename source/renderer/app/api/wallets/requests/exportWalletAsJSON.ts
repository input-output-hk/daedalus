import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/requestV0';

export type ExportWalletAsJSONParams = {
  walletId: string;
  filePath: string;
};
export const exportWalletAsJSON = (
  config: RequestConfig,
  { walletId, filePath }: ExportWalletAsJSONParams
): Promise<[]> =>
  request(
    {
      method: 'POST',
      path: `/api/backup/export/${walletId}`,
      ...config,
    },
    {},
    filePath
  );
