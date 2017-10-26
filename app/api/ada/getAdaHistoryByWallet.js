// @flow
import type { ApiTransactions } from 'daedalus-client-api';
import { request } from './lib/request';

export type GetAdaHistoryByWalletQueryParams = {
  walletId: string,
  skip: number,
  limit: number,
};

export const getAdaHistoryByWallet = (
  ca: string, pathParams: {}, queryParams: GetAdaHistoryByWalletQueryParams
): Promise<ApiTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/txs/histories',
    port: 8090,
    ca,
  }, queryParams)
);
