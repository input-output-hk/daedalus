// @flow
import type { AdaTransactionFee } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type AdaTxFeeParams = {
  ca: string,
  sender: string,
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
    port: environment.WALLET_PORT,
    ca,
  }, {}, { groupingPolicy })
);
