import { observable, makeObservable } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  TransactionAddresses,
  TransactionType,
  TransactionState,
  TransactionWithdrawalType,
} from '../api/transactions/types';
import type { Tokens } from '../api/assets/types';
import type { TransactionMetadata } from '../types/TransactionMetadata';

// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
export const TransactionStates: EnumMap<string, TransactionState> = {
  PENDING: 'pending',
  OK: 'in_ledger',
  FAILED: 'expired',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
export const TransactionTypes: EnumMap<string, TransactionType> = {
  CARD: 'card',
  EXPEND: 'expend',
  INCOME: 'income',
  EXCHANGE: 'exchange',
};
export const TransactionWithdrawal: TransactionWithdrawalType = 'self';
export class WalletTransaction {
  id = '';
  type: TransactionType;
  title = '';
  amount: BigNumber;
  fee: BigNumber;
  deposit: BigNumber;
  assets: Tokens;
  date: Date | null | undefined;
  description = '';
  addresses: TransactionAddresses = {
    from: [],
    to: [],
    withdrawals: [],
  };
  state: TransactionState;
  confirmations: number;
  slotNumber: number | null | undefined;
  epochNumber: number | null | undefined;
  metadata: TransactionMetadata | null | undefined;

  constructor(data: {
    id: string;
    type: TransactionType;
    title: string;
    amount: BigNumber;
    fee: BigNumber;
    deposit: BigNumber;
    date: Date | null | undefined;
    assets: Tokens;
    description: string;
    addresses: TransactionAddresses;
    state: TransactionState;
    confirmations: number;
    slotNumber: number | null | undefined;
    epochNumber: number | null | undefined;
    metadata: TransactionMetadata | null | undefined;
  }) {
    makeObservable(this, {
      id: observable,
      type: observable,
      title: observable,
      amount: observable,
      fee: observable,
      deposit: observable,
      assets: observable,
      date: observable,
      description: observable,
      addresses: observable,
      state: observable,
      confirmations: observable,
      slotNumber: observable,
      epochNumber: observable,
      metadata: observable,
    });

    Object.assign(this, data);
  }
}
