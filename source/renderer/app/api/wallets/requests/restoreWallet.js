// @flow
import type { RequestConfig } from '../../common/types';
import type { WalletInitData } from './createWallet';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export const restoreWallet = (
  config: RequestConfig,
  { walletInitData }: { walletInitData: WalletInitData }
): Promise<AdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/api/v1/wallets',
      ...config,
    },
    {},
    walletInitData
  );
