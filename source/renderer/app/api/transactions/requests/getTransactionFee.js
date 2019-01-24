// @flow
import type { RequestConfig } from '../../common/types';
import type { TransactionParams } from './createTransaction';
import type { TransactionFee } from '../types';
import { request } from '../../utils/request';

export const getTransactionFee = (
  config: RequestConfig,
  { data }: TransactionParams
): Promise<TransactionFee> => (
  request({
    method: 'POST',
    path: '/api/v1/transactions/fees',
    ...config,
  }, {}, data)
);
