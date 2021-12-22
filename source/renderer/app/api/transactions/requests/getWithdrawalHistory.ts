import type { RequestConfig } from '../../common/types';
import type { Transactions } from '../types';
import { request } from '../../utils/request';

export const getWithdrawalHistory = (
  config: RequestConfig,
  walletId: string
): Promise<Transactions> =>
  request(
    {
      method: 'GET',
      path: `/v2/wallets/${walletId}/transactions`,
      ...config,
    },
    {
      minWithdrawal: 1,
    },
    null
  );
