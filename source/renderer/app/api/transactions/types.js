// @flow
import { WalletTransaction } from '../../domains/WalletTransaction';

export type TransactionAmount = {
  quantity: number,
  unit: 'lovelace',
};

export type TransactionDepth = {
  quantity: number,
  unit: 'block',
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
  pending_since?: {
    time: Date,
    block: {
      ...TransactionInsertionBlock,
      height: {
        quantity: number,
        unit: string,
      },
    },
  },
  depth: TransactionDepth,
  direction: 'outgoing' | 'incoming',
  inputs: Array<TransactionInputs>,
  outputs: Array<TransactionOutputs>,
  status: TransactionState,
};

export type Transactions = Array<Transaction>;

export type TransactionInputs = {
  address: string,
  amount?: TransactionAmount,
  id: string,
  index: number,
};

export type TransactionOutputs = {
  address: string,
  amount: TransactionAmount,
};

export type TransactionState = 'pending' | 'in_ledger';

export type TrasactionAddresses = { from: Array<string>, to: Array<string> };

export type TransactionType = 'card' | 'expend' | 'income' | 'exchange';

// Req / Res Transaction Types
export type GetTransactionsRequest = {
  walletId: string,
  order?: 'ascending' | 'descending',
  fromDate: ?string,
  toDate: ?string,
  isLegacy: boolean,
  // @API TODO - Params "pending" for V2
  // searchTerm: string,
  // skip: number,
  // limit: number,
  // isFirstLoad: boolean,
  // isRestoreActive: boolean,
  // isRestoreCompleted: boolean,
  // cachedTransactions: Array<WalletTransaction>,
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
  passphrase: string,
};

export type DeleteTransactionRequest = {
  walletId: string,
  transactionId: string,
  isLegacy: boolean,
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
