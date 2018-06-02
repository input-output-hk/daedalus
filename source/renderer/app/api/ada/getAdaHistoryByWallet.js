// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';

export type GetAdaHistoryByWalletParams = {
  walletId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByWallet = (
  { walletId, skip, limit }: GetAdaHistoryByWalletParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
  }, { walletId, skip, limit })
);
