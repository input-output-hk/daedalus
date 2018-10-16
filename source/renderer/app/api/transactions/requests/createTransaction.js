// @flow
import type { RequestConfig } from '../../common/types';
import type { Transaction, PaymentDistribution } from '../types';
import { request } from '../../utils/request';

export type TransactionParams = {
  data: {
    source: {
      accountIndex: number,
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
): Promise<Transaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions',
    ...config,
  }, {}, data)
);
