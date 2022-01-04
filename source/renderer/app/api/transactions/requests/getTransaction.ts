import type { RequestConfig } from '../../common/types';
import type { Transaction } from '../types';
import { request } from '../../utils/request';

export const getTransaction = (
  config: RequestConfig,
  walletId: string,
  transactionId: string
): Promise<Transaction> =>
  request({
    method: 'GET',
    path: `/v2/wallets/${walletId}/transactions/${transactionId}`,
    ...config,
  });
