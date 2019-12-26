// @flow
import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallet, LegacyWalletInitData } from '../types';
import { request } from '../../utils/request';

export const restoreLegacyWallet = (
  config: RequestConfig,
  {
    walletInitData,
    isLedger,
  }: { walletInitData: LegacyWalletInitData, isLedger?: boolean }
): Promise<LegacyAdaWallet> => {
  const queryParams = isLedger ? { seed_generation: 'ledger' } : {};

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
