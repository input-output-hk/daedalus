// @flow
import type { AdaTransactions } from './types';
import { request } from './lib/v1/request';

export type GetAdaHistoryByWalletParams = {
  ca: string,
  wallet_id: string,
  page: number,
  per_page: number,
  accountIndex: number,
};

export const getAdaHistoryByWallet = (
  { ca, ...requestParams }:
    GetAdaHistoryByWalletParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/transactions',
    port: 8090,
    ca,
  }, requestParams)
);
