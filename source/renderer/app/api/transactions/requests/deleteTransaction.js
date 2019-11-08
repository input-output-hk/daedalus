// @flow
import type { RequestConfig } from '../../common/types';
import type { DeleteTransactionRequest } from '../types';
import { request } from '../../utils/request';

export const deleteTransaction = (
  config: RequestConfig,
  { walletId, transactionId }: DeleteTransactionRequest
): Promise<*> =>
  request({
    method: 'DELETE',
    path: `/v2/byron-wallets/${walletId}/transactions/${transactionId}`,
    ...config,
  });
