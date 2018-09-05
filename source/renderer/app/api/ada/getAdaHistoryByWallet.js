// @flow
import type { AdaTransactionsV1, RequestConfig } from './types';
import { request } from './lib/v1/request';

export type GetAdaHistoryByWalletParams = {
  wallet_id: string,
  page: number,
  per_page: number,
  accountIndex: number,
  sort_by: string,
};

const requestOptions = {
  returnMeta: true,
};

export const getAdaHistoryByWallet = (
  config: RequestConfig,
  { ...requestParams }: GetAdaHistoryByWalletParams
): Promise<AdaTransactionsV1> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/v1/transactions',
    ...config,
  }, requestParams, null, requestOptions)
);
