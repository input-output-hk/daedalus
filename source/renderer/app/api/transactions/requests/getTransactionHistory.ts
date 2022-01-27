import type { RequestConfig } from '../../common/types';
import type { Transactions } from '../types';
import { request } from '../../utils/request';

export type GetTransactionsQueryParams = {
  start?: string;
  end?: string;
  order: 'ascending' | 'descending';
};
export const getTransactionHistory = (
  config: RequestConfig,
  walletId: string,
  { ...queryParams }: GetTransactionsQueryParams
): Promise<Transactions> =>
  request(
    {
      method: 'GET',
      path: `/v2/wallets/${walletId}/transactions`,
      ...config,
    },
    queryParams,
    null
  );
