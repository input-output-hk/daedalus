// @flow
import type { ApiTransactions } from './types';
import { request } from './lib/request';

export type getAdaAddressHistoryParams = {
  ca: string,
  accountId: string,
  address: string,
  skip: number,
  limit: number,
};

export const getAdaAddressHistory = (
  { ca, accountId, address, skip, limit }: getAdaAddressHistoryParams
): Promise<ApiTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: 8090,
    ca,
  }, { accountId, address, skip, limit })
);
