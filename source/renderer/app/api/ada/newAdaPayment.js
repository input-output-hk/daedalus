// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';

export type NewAdaPaymentParams = {
  ca: string,
  sender: string,
  receiver: string,
  amount: string,
  password: ?string,
  // "groupingPolicy" - Spend everything from the address
  // "OptimizeForSize" for no grouping
  groupingPolicy: ?'OptimizeForSecurity' | 'OptimizeForSize',
};


export const newAdaPayment = (
  { ca, sender, receiver, amount, groupingPolicy, password }: NewAdaPaymentParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/txs/payments/${sender}/${receiver}/${amount}`,
    port: 8090,
    ca,
  }, { passphrase: password }, { groupingPolicy })
);
