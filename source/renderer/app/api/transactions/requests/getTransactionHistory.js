// @flow
import type { RequestConfig } from '../../common/types';
import type { Transactions } from '../types';
import { request } from '../../utils/request';

export type GetTxnHistoryParams = {
  wallet_id: string,
  account_index: number,
  page: number,
  per_page: number,
  sort_by: string,
};

const requestOptions = {
  returnMeta: true,
};

export const getTransactionHistory = (
  config: RequestConfig,
  { ...requestParams }: GetTxnHistoryParams
): Promise<Transactions> => (
  request({
    method: 'GET',
    path: '/api/v1/transactions',
    ...config,
  }, requestParams, null, requestOptions)
);
