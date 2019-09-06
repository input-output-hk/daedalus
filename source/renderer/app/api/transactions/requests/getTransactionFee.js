// @flow
import type { RequestConfig } from '../../common/types';
import type { TransactionFee, GetTransactionFeeRequest } from '../types';
import { request } from '../../utils/request';

export const getTransactionFee = (
  config: RequestConfig,
  { walletId, data }: GetTransactionFeeRequest
): Promise<TransactionFee> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/transactions/fees`,
      ...config,
    },
    {},
    data
  );
