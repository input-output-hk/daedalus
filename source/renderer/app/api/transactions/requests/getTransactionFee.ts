import type { RequestConfig } from '../../common/types';
import type { TransactionFee, GetTransactionFeeParams } from '../types';
import { request } from '../../utils/request';

export const getTransactionFee = (
  config: RequestConfig,
  { walletId, data }: GetTransactionFeeParams
): Promise<TransactionFee> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/payment-fees`,
      ...config,
    },
    {},
    data
  );
