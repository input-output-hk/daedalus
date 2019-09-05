// @flow
import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';
import type { GetWalletUtxosRequest, WalletUtxos } from '../types';

export const getWalletUtxos = (
  config: RequestConfig,
  { walletId }: GetWalletUtxosRequest
): Promise<WalletUtxos> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/statistics/utxos`,
    ...config,
  });
