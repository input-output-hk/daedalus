// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';

export type GetAdaHistoryByAccountParams = {
  accountId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByAccount = (
  { accountId, skip, limit }: GetAdaHistoryByAccountParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
  }, { accountId, skip, limit })
);
