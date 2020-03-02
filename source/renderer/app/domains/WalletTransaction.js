// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  TrasactionAddresses,
  TransactionType,
  TransactionDepth,
  TransactionState,
  TransactionStatePending,
  TransactionStateOk,
} from '../api/transactions/types';

export const TransactionStates: {
  PENDING: TransactionStatePending,
  OK: TransactionStateOk,
} = {
  PENDING: 'pending',
  OK: 'in_ledger',
  IN_LEDGER: 'in_ledger',
};

export const TransactionTypes: {
  CARD: TransactionType,
  EXPEND: TransactionType,
  INCOME: TransactionType,
  EXCHANGE: TransactionType,
} = {
  CARD: 'card',
  EXPEND: 'expend',
  INCOME: 'income',
  EXCHANGE: 'exchange',
};

export class WalletTransaction {
  @observable id: string = '';
  @observable type: TransactionType;
  @observable title: string = '';
  @observable amount: BigNumber;
  @observable date: ?Date;
  @observable description: string = '';
  @observable addresses: TrasactionAddresses = { from: [], to: [] };
  @observable state: TransactionState;
  @observable depth: TransactionDepth;
  @observable slotNumber: ?number;
  @observable epochNumber: ?number;

  constructor(data: {
    id: string,
    type: TransactionType,
    title: string,
    amount: BigNumber,
    date: ?Date,
    description: string,
    addresses: TrasactionAddresses,
    state: TransactionState,
    depth: TransactionDepth,
    slotNumber: ?number,
    epochNumber: ?number,
  }) {
    Object.assign(this, data);
  }
}
