// @flow
import type { ApiTransactions } from 'daedalus-client-api';
import { request } from './lib/request';

export type GetAdaHistoryByAccountQueryParams = {
  accountId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByAccount = (
  ca: string, pathParams: {}, queryParams: GetAdaHistoryByAccountQueryParams
): Promise<ApiTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: 8090,
    ca,
  }, queryParams)
);
