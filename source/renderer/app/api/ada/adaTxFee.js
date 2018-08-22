// @flow
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

export const adaTxFee = (
  { ca, sender, receiver, amount, groupingPolicy }: AdaTxFeeParams
): Promise<AdaTransactionFee> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/txs/fee/${sender}/${receiver}/${amount}`,
    port: 8090,
    ca,
  }, {}, { groupingPolicy })
);
