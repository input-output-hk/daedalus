// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  TrasactionAddresses,
  TransactionType,
  TransactionDepth,
} from '../api/transactions/types';

export type TransactionState = 'pending' | 'failed' | 'ok';

export const TransactionStates: {
  PENDING: TransactionState,
  FAILED: TransactionState,
  OK: TransactionState,
} = {
  PENDING: 'pending',
  FAILED: 'failed',
  OK: 'ok',
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
