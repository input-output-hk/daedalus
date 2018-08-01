// @flow
import type { AdaTransactionFee } from './types';
import { request } from './lib/v1/request';

export type AdaTxFeeParams = {
  ca: string,
  data: {
    source: {
      accountIndex: number,
      walletId: string,
    },
    destinations: [
      {
        address: string,
        amount: number,
      },
    ],
    groupingPolicy: ?'OptimizeForSecurity' | 'OptimizeForSize',
    spendingPassword: ?string
  },
};

export const adaTxFee = (
  { ca, data }: AdaTxFeeParams
): Promise<AdaTransactionFee> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/fees',
    port: 8090,
    ca,
  }, {}, data)
);
