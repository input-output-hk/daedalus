// @flow
import type { RequestConfig } from '../../common/types';
import type { TransactionPaymentData, TransactionFee } from '../types';
import { request } from '../../utils/request';

export type GetTransactionFeeParams = {
  walletId: string,
  data: {
    payments: Array<TransactionPaymentData>,
  },
};

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
