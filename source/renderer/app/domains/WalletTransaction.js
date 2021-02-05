// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  TrasactionAddresses,
  TransactionType,
  TransactionDepth,
  TransactionState,
  TransactionWithdrawalType,
} from '../api/transactions/types';
import type { TransactionMetadata } from '../types/TransactionMetadata';

export const TransactionStates: EnumMap<string, TransactionState> = {
  PENDING: 'pending',
  OK: 'in_ledger',
  IN_LEDGER: 'in_ledger',
  FAILED: 'expired',
};

export const TransactionTypes: EnumMap<string, TransactionType> = {
  CARD: 'card',
  EXPEND: 'expend',
  INCOME: 'income',
  EXCHANGE: 'exchange',
};

export const TransactionWithdrawal: TransactionWithdrawalType = 'self';

export class WalletTransaction {
  @observable id: string = '';
  @observable type: TransactionType;
  @observable title: string = '';
  @observable amount: BigNumber;
  @observable fee: BigNumber;
  @observable deposit: BigNumber;
  @observable date: ?Date;
  @observable description: string = '';
  @observable addresses: TrasactionAddresses = {
    from: [],
    to: [],
    withdrawals: [],
  };
  @observable state: TransactionState;
  @observable depth: TransactionDepth;
  @observable slotNumber: ?number;
  @observable epochNumber: ?number;
  @observable metadata: ?TransactionMetadata;

  constructor(data: {
    id: string,
    type: TransactionType,
    title: string,
    amount: BigNumber,
    fee: BigNumber,
    deposit: BigNumber,
    date: ?Date,
    description: string,
    addresses: TrasactionAddresses,
    state: TransactionState,
    depth: TransactionDepth,
    slotNumber: ?number,
    epochNumber: ?number,
    metadata: ?TransactionMetadata,
  }) {
    Object.assign(this, data);
  }
}
