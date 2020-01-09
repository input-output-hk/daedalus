// @flow
import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallet, LegacyWalletInitData } from '../types';
import { request } from '../../utils/request';

export const restoreByronWallet = (
  config: RequestConfig,
  { walletInitData }: { walletInitData: LegacyWalletInitData },
  type: string
): Promise<LegacyAdaWallet> =>
  request(
    {
      method: 'POST',
      path: `/v2/byron-wallets/${type}`,
      ...config,
    },
    {},
    walletInitData
  );
