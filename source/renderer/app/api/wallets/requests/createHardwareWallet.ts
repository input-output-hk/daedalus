import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

type WalletInitData = {
  name: string;
  account_public_key: string;
};
export const createHardwareWallet = (
  config: RequestConfig,
  {
    walletInitData,
  }: {
    walletInitData: WalletInitData;
  }
): Promise<AdaWallet> => {
  return request(
    {
      method: 'POST',
      path: '/v2/wallets',
      ...config,
    },
    {},
    walletInitData
  );
};
