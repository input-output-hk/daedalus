// @flow
import type { RequestConfig } from '../../common/types';
import type { TransactionFee, TransactionPaymentData } from '../types';
import { request } from '../../utils/request';

export type GetTransactionFeeRequest = {
  walletId: string,
  data: {
    payments: Array<TransactionPaymentData>,
  },
};

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
