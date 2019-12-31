// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet, LegacyWalletInitData } from '../types';
import { request } from '../../utils/request';

export const restoreByronIcarusWallet = (
  config: RequestConfig,
  { walletInitData }: { walletInitData: LegacyWalletInitData }
): Promise<AdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/v2/byron-wallets/icarus',
      ...config,
    },
    {},
    walletInitData
  );
