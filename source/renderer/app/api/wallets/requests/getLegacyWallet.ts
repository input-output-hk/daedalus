import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallet } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const getLegacyWallet = (
  config: RequestConfig,
  {
    walletId,
  }: {
    walletId: string;
  }
): Promise<LegacyAdaWallet> =>
  request({
    method: 'GET',
    path: `/v2/byron-wallets/${getRawWalletId(walletId)}`,
    ...config,
  });
