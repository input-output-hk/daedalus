// @flow
import type { AdaTransactions, RequestConfig } from './types';
import { request } from './lib/request';

export type GetAdaAddressHistoryParams = {
  accountId: string,
  address: string,
  skip: number,
  limit: number,
};

export const getAdaAddressHistory = (
  config: RequestConfig,
  { accountId, address, skip, limit }: GetAdaAddressHistoryParams
): Promise<AdaTransactions> => (
  request(Object.assign({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
  }, config), { accountId, address, skip, limit })
);
