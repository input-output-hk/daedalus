// @flow
import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallet, LegacyWalletInitData } from '../types';
import { request } from '../../utils/request';

export const restoreLegacyWallet = (
  config: RequestConfig,
  {
    walletInitData,
  }: { walletInitData: LegacyWalletInitData, isLedger?: boolean }
): Promise<LegacyAdaWallet> => {
  const queryParams = { };

  return request(
    {
      method: 'POST',
      path: '/v2/byron-wallets',
      ...config,
    },
    queryParams,
    walletInitData
  );
};
