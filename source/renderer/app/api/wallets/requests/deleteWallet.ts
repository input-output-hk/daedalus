import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

export const deleteWallet = (
  config: RequestConfig,
  {
    walletId,
  }: {
    walletId: string;
  }
): Promise<any> =>
  request({
    method: 'DELETE',
    path: `/v2/wallets/${walletId}`,
    ...config,
  });
