// @flow
import type { RequestConfig } from '../../common/types';
import type { Transaction, PaymentDistribution } from '../types';
import { request } from '../../utils/request';

export type TransactionParams = {
  data: {
    source: {
      walletId: string,
    },
    destinations: Array<PaymentDistribution>,
    groupingPolicy: ?'OptimizeForSecurity' | 'OptimizeForSize',
    spendingPassword?: string,
  },
};

export const createTransaction = (
  config: RequestConfig,
  { data }: TransactionParams
): Promise<Transaction> =>
  request(
    {
      method: 'POST',
      path: '/api/v1/transactions',
      ...config,
    },
    {},
    data
  );
