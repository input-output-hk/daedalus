import type { RequestConfig } from '../../common/types';
import type { AdaWallet, WalletInitData } from '../types';
import { request } from '../../utils/request';

export const createWallet = (
  config: RequestConfig,
  {
    walletInitData,
  }: {
    walletInitData: WalletInitData;
  }
): Promise<AdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/v2/wallets',
      ...config,
    },
    {},
    walletInitData
  );
