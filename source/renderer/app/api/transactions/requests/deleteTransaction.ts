import type { RequestConfig } from '../../common/types';
import { request } from '../../utils/request';

export const deleteTransaction = (
  config: RequestConfig,
  {
    walletId,
    transactionId,
  }: {
    walletId: string;
    transactionId: string;
  }
): Promise<any> =>
  request({
    method: 'DELETE',
    path: `/v2/wallets/${walletId}/transactions/${transactionId}`,
    ...config,
  });
