// @flow
import type { ApiTransactions } from 'daedalus-client-api';
import { request } from './lib/request';

export type GetAdaHistoryQueryParams = {
  walletId: ?string,
  accountId: ?string,
  address: ?string,
  skip: number,
  limit: number,
};

export const getAdaHistory = (
  ca: string, pathParams: {}, queryParams: GetAdaHistoryQueryParams
): Promise<ApiTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: 8090,
    ca,
  }, queryParams)
);
