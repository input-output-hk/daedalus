import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export const updateWallet = (
  config: RequestConfig,
  {
    walletId,
    name,
  }: {
    walletId: string;
    name: string;
  }
): Promise<AdaWallet> =>
  request(
    {
      method: 'PUT',
      path: `/v2/wallets/${walletId}`,
      ...config,
    },
    {},
    {
      name,
    }
  );
