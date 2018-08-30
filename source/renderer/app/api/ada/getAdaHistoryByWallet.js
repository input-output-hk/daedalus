// @flow
import type { AdaTransactions, RequestConfig } from './types';
import { request } from './lib/request';

export type GetAdaHistoryByWalletParams = {
  walletId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByWallet = (
  config: RequestConfig,
  { walletId, skip, limit }: GetAdaHistoryByWalletParams
): Promise<AdaTransactions> => (
  request(Object.assign({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
  }, config), { walletId, skip, limit })
);
