// @flow
import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallet, LegacyWalletInitData } from '../types';
import { request } from '../../utils/request';

export const restoreLegacyWallet = (
  config: RequestConfig,
  { walletInitData }: { walletInitData: LegacyWalletInitData }
): Promise<LegacyAdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/v2/byron-wallets',
      ...config,
    },
    {},
    walletInitData
  );

export const restoreYoroiLegacyWallet = (
  config: RequestConfig,
  { walletInitData }: { walletInitData: LegacyWalletInitData }
): Promise<LegacyAdaWallet> =>
  request(
    {
      method: 'POST',
      // @todo - change to proper endpoint path
      path: '/v2/byron-wallets',
      ...config,
    },
    {},
    walletInitData
  );
