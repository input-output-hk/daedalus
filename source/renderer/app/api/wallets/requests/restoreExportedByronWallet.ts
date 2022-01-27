import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallet } from '../types';
import type { ExportedByronWallet } from '../../../types/walletExportTypes';
import { request } from '../../utils/request';

export const restoreExportedByronWallet = (
  config: RequestConfig,
  {
    walletInitData,
  }: {
    walletInitData: ExportedByronWallet;
  }
): Promise<LegacyAdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/v2/byron-wallets',
      ...config,
    },
    {},
    { ...walletInitData, style: 'random' }
  );
