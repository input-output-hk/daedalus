// @flow
import WalletTransaction from '../../domains/WalletTransaction';
import type { ResponseBase } from '../common/types';

export type Transactions = {
  data: Array<Transaction>,
  ...ResponseBase
};

export type Transaction = {
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

export type TransactionFee = {
  estimatedAmount: number,
  ...ResponseBase
};

export type TrasactionAddresses = { from: Array<string>, to: Array<string> };
export type TransactionType = 'card' | 'expend' | 'income' | 'exchange';

// req/res Transaction Types
export type GetTransactionsRequest = {
  walletId: string,
  searchTerm: string,
  skip: number,
  limit: number,
};

export type TransactionRequest = {
  accountIndex: number,
  walletId: string,
  address: string,
  amount: number,
  spendingPassword?: ?string,
};

export type GetTransactionsResponse = {
  transactions: Array<WalletTransaction>,
  total: number,
};
