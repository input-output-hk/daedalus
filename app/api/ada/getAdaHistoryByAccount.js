// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/request';

export type GetAdaHistoryByAccountParams = {
  ca: string,
  accountId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByAccount = (
  { ca, accountId, skip, limit }: GetAdaHistoryByAccountParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: 8090,
    ca,
  }, { accountId, skip, limit })
);
