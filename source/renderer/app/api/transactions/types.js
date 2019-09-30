// @flow
import { WalletTransaction } from '../../domains/WalletTransaction';
import type { ResponseBase } from '../common/types';

export type Transactions = ResponseBase & {
  data: Array<Transaction>,
};

export type TransactionAmount = {
  quantity: number,
  unit: 'lovelace',
};

export type TransactionDepth = {
  quantity: number,
  unit: 'slot',
};

export type TransactionInsertionBlock = {
  slot_number: number,
  epoch_number: number,
};

export type Transaction = {
  id: string,
  amount: TransactionAmount,
  inserted_at?: {
    time: Date,
    block: TransactionInsertionBlock,
  },
  depth: TransactionDepth,
  direction: 'outgoing' | 'incoming',
  inputs: Array<TransactionInputs>,
  outputs: Array<TransactionOutputs>,
  status: TransactionState,
};

export type TransactionInputs = {
  address: string,
  amount: TransactionAmount,
  id: string,
  index: number,
};

export type TransactionOutputs = {
  address: string,
  amount: TransactionAmount,
};

export type TransactionState = 'pending' | 'in_ledger' | 'invalidated';

export type TrasactionAddresses = { from: Array<string>, to: Array<string> };

export type TransactionType = 'card' | 'expend' | 'income' | 'exchange';

// req/res Transaction Types
export type GetTransactionsRequest = {
  walletId: string,
  searchTerm: string,
  skip: number,
  limit: number,
  isFirstLoad: boolean,
  isRestoreActive: boolean,
  isRestoreCompleted: boolean,
  cachedTransactions: Array<WalletTransaction>,
};

export type GetTransactionFeeRequest = {
  walletId: string,
  address: string,
  amount: number,
};

export type CreateTransactionRequest = {
  walletId: string,
  address: string,
  amount: number,
  spendingPassword: string,
};

export type GetTransactionsResponse = {
  transactions: Array<WalletTransaction>,
  total: number,
};

export type TransactionFeeAmount = {
  quantity: number,
  unit: 'lovelace',
};

export type TransactionPaymentData = {
  address: string,
  amount: TransactionFeeAmount,
};

export type TransactionFee = {
  amount: TransactionFeeAmount,
};
