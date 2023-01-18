import type { RequestConfig } from '../../common/types';
import type { Transactions } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export type GetTransactionsQueryParams = {
  start?: string;
  end?: string;
  order: 'ascending' | 'descending';
};
export const getLegacyWalletTransactionHistory = (
  config: RequestConfig,
  walletId: string,
  { ...queryParams }: GetTransactionsQueryParams
): Promise<Transactions> =>
  request(
    {
      method: 'GET',
      path: `/v2/byron-wallets/${getRawWalletId(walletId)}/transactions`,
      ...config,
    },
    queryParams,
    null
  );
