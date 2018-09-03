// @flow
import type { AdaTransactions, RequestConfig } from './types';
import { request } from './lib/request';

export type GetAdaHistoryParams = {
  walletId: ?string,
  accountId: ?string,
  address: ?string,
  skip: number,
  limit: number,
};

export const getAdaHistory = (
  config: RequestConfig,
  { walletId, accountId, address, skip, limit }: GetAdaHistoryParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    ...config,
  }, { walletId, accountId, address, skip, limit })
);
