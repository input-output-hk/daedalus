// @flow
import type { AdaTransactions, RequestConfig } from './types';
import { request } from './lib/v1/request';

export type GetAdaHistoryByWalletParams = {
  wallet_id: string,
  page: number,
  per_page: number,
  accountIndex: number,
  sort_by: string,
};

export const getAdaHistoryByWallet = (
  config: RequestConfig,
  { ...requestParams }: GetAdaHistoryByWalletParams
): Promise<AdaTransactions> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/transactions',
    ...config,
  }, requestParams)
);
