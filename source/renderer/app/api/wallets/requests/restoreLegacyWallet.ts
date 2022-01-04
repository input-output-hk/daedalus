import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallet, LegacyWalletInitData } from '../types';
import { request } from '../../utils/request';

export const restoreLegacyWallet = (
  config: RequestConfig,
  {
    walletInitData,
  }: {
    walletInitData: LegacyWalletInitData;
  },
  type = ''
): Promise<LegacyAdaWallet> => {
  const queryParams = {};
  return request(
    {
      method: 'POST',
      path: `/v2/byron-wallets${type}`,
      ...config,
    },
    queryParams,
    walletInitData
  );
};
