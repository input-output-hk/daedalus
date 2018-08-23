// @flow
<<<<<<< HEAD
import type { AdaTransactionFee, AdaTxFeeParams } from './types';
import { request } from './lib/v1/request';
=======
import type { AdaTransactionFee } from './types';
import { request } from './lib/request';

export type AdaTxFeeParams = {
  ca: string,
  sender: number, // v1 API shows this: sender: accountIndex -> type is number
  receiver: string,
  amount: string,
  // "groupingPolicy" - Spend everything from the address
  // "OptimizeForSize" for no grouping
  groupingPolicy: ?'OptimizeForSecurity' | 'OptimizeForSize',
};
>>>>>>> a13312cf6dd1a50d1540a07a566ea79b291137b4

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
