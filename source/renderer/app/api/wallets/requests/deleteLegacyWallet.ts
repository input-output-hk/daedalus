import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const deleteLegacyWallet = (
  config: RequestConfig,
  {
    walletId,
  }: {
    walletId: string;
  }
): Promise<any> =>
  request({
    method: 'DELETE',
    path: `/v2/byron-wallets/${getRawWalletId(walletId)}`,
    ...config,
  });
