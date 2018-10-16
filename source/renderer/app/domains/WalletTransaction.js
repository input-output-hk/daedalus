// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type { WalletAssuranceMode } from '../api/wallets/types';
import type {
  TxnAssuranceLevel,
  TransactionState,
  TrasactionAddresses,
  TransactionType
} from '../api/transactions/types';

export const transactionStates: {
  PENDING: TransactionState, FAILED: TransactionState, OK: TransactionState,
} = {
  PENDING: 'pending', FAILED: 'failed', OK: 'ok',
};

export const TxnAssuranceLevelOptions: {
  LOW: TxnAssuranceLevel, MEDIUM: TxnAssuranceLevel, HIGH: TxnAssuranceLevel,
} = {
  LOW: 'low', MEDIUM: 'medium', HIGH: 'high',
};

export const transactionTypes: {
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

export default class WalletTransaction {

  @observable id: string = '';
  @observable type: TransactionType;
  @observable title: string = '';
  @observable amount: BigNumber;
  @observable date: Date;
  @observable description: string = '';
  @observable numberOfConfirmations: number = 0;
  @observable addresses: TrasactionAddresses = { from: [], to: [] };
  @observable state: TransactionState;

  constructor(data: {
    id: string,
    type: TransactionType,
    title: string,
    amount: BigNumber,
    date: Date,
    description: string,
    numberOfConfirmations: number,
    addresses: TrasactionAddresses,
    state: TransactionState,
  }) {
    Object.assign(this, data);
  }

  getAssuranceLevelForMode(mode: WalletAssuranceMode): TxnAssuranceLevel {
    if (this.numberOfConfirmations < mode.low) {
      return TxnAssuranceLevelOptions.LOW;
    } else if (this.numberOfConfirmations < mode.medium) {
      return TxnAssuranceLevelOptions.MEDIUM;
    }
    return TxnAssuranceLevelOptions.HIGH;
  }

}
