// @flow
import type { RequestConfig } from '../../common/types';
import type { GetWalletUtxosRequest, WalletUtxos } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export const getByronWalletUtxos = (
  config: RequestConfig,
  { walletId }: GetWalletUtxosRequest
): Promise<WalletUtxos> =>
  request({
    method: 'GET',
    path: `/v2/byron-wallets/${getRawWalletId(walletId)}/statistics/utxos`,
    ...config,
  });
