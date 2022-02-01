import type { RequestConfig } from '../../common/types';
import type { Transaction, TransactionParams } from '../types';
import { request } from '../../utils/request';

export const createTransaction = (
  config: RequestConfig,
  { walletId, data }: TransactionParams
): Promise<Transaction> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/transactions/`,
      ...config,
    },
    {},
    data
  );
