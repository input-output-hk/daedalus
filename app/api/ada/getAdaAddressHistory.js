// @flow
import type { ApiTransactions } from 'daedalus-client-api';
import { request } from './lib/request';

export type GetAdaAddressHistoryQueryParams = {
  accountId: string,
  address: string,
  skip: number,
  limit: number,
};

export const getAdaAddressHistory = (
  ca: string, pathParams: {}, queryParams: GetAdaAddressHistoryQueryParams
): Promise<ApiTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: 8090,
    ca,
  }, queryParams)
);
