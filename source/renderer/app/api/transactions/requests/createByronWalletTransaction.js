// @flow
import type { RequestConfig } from '../../common/types';
import type { Transaction, TransactionPaymentData } from '../types';
import { request } from '../../utils/request';

export type TransactionParams = {
  walletId: string,
  data: {
    payments: Array<TransactionPaymentData>,
    passphrase: string,
  },
};

export const createByronWalletTransaction = (
  config: RequestConfig,
  { walletId, data }: TransactionParams
): Promise<Transaction> =>
  request(
    {
      method: 'POST',
      path: `/v2/byron-wallets/${walletId}/transactions/`,
      ...config,
    },
    {},
    data
  );
