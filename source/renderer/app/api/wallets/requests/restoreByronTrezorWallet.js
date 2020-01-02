// @flow
import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallet, LegacyWalletInitData } from '../types';
import { request } from '../../utils/request';

export const restoreByronTrezorWallet = (
  config: RequestConfig,
  { walletInitData }: { walletInitData: LegacyWalletInitData }
): Promise<LegacyAdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/v2/byron-wallets/trezor',
      ...config,
    },
    {},
    walletInitData
  );
