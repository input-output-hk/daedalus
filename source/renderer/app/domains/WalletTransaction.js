// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  TransactionAddresses,
  TransactionType,
  TransactionState,
  TransactionWithdrawalType,
} from '../api/transactions/types';
import type { Tokens } from '../api/assets/types';
import type { TransactionMetadata } from '../types/TransactionMetadata';

export const TransactionStates: EnumMap<string, TransactionState> = {
  PENDING: 'pending',
  OK: 'in_ledger',
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
  @observable assets: Tokens;
  @observable date: ?Date;
  @observable description: string = '';
  @observable addresses: TransactionAddresses = {
    from: [],
    to: [],
    withdrawals: [],
  };
  @observable state: TransactionState;
  @observable confirmations: number;
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
    assets: Tokens,
    description: string,
    addresses: TransactionAddresses,
    state: TransactionState,
    confirmations: number,
    slotNumber: ?number,
    epochNumber: ?number,
    metadata: ?TransactionMetadata,
  }) {
    Object.assign(this, data);
  }
}
