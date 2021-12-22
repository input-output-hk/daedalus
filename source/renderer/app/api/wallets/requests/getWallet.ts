import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export const getWallet = (
  config: RequestConfig,
  {
    walletId,
  }: {
    walletId: string;
  }
): Promise<AdaWallet> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}`,
    ...config,
  });
