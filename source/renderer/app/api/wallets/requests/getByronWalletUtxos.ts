import type { RequestConfig } from '../../common/types';
import type { WalletUtxos } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const getByronWalletUtxos = (
  config: RequestConfig,
  {
    walletId,
  }: {
    walletId: string;
  }
): Promise<WalletUtxos> =>
  request({
    method: 'GET',
    path: `/v2/byron-wallets/${getRawWalletId(walletId)}/statistics/utxos`,
    ...config,
  });
