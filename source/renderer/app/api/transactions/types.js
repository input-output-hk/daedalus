// @flow
import WalletTransaction from '../../domains/WalletTransaction';
import type { ResponseBase } from '../common/types';

export type AdaTransactions = {
  data: Array<AdaTransaction>,
  ...ResponseBase
};

export type AdaTransaction = {
  amount: number,
  confirmations: number,
  creationTime: string,
  direction: 'outgoing' | 'incoming',
  id: string,
  type: 'local' | 'foreign',
  inputs: Array<PaymentDistribution>,
  outputs: Array<PaymentDistribution>,
  status: {
    tag: 'applying' | 'inNewestBlocks' | 'persisted' | 'wontApply' | 'creating',
    data: {},
  },
};

export type PaymentDistribution = {
  address: string,
  amount: number
};

export type TxnAssuranceLevel = 'low' | 'medium' | 'high';
export type TransactionState = 'pending' | 'failed' | 'ok';

export type AdaTransactionFee = {
  estimatedAmount: number,
  ...ResponseBase
};

export type AdaTransactionParams = {
  data: {
    source: {
      accountIndex: number,
      walletId: string,
    },
    destinations: Array<PaymentDistribution>,
    groupingPolicy: ?'OptimizeForSecurity' | 'OptimizeForSize',
    spendingPassword: ?string
  },
};

export type AdaTxFeeParams = AdaTransactionParams;

// I/O Transaction Types
export type GetTransactionsRequest = {
  walletId: string,
  searchTerm: string,
  skip: number,
  limit: number,
};

export type GetTransactionsResponse = {
  transactions: Array<WalletTransaction>,
  total: number,
};
