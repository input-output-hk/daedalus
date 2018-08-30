// @flow
import type { AdaTransactions, RequestConfig } from './types';
import { request } from './lib/request';

export type GetAdaHistoryByAccountParams = {
  accountId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByAccount = (
  config: RequestConfig,
  { accountId, skip, limit }: GetAdaHistoryByAccountParams
): Promise<AdaTransactions> => (
  request(Object.assign({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
  }, config), { accountId, skip, limit })
);
