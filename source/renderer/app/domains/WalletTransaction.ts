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
  @observable
  id = '';
  @observable
  type: TransactionType;
  @observable
  title = '';
  @observable
  amount: BigNumber;
  @observable
  fee: BigNumber;
  @observable
  deposit: BigNumber;
  @observable
  assets: Tokens;
  @observable
  date: Date | null | undefined;
  @observable
  description = '';
  @observable
  addresses: TransactionAddresses = {
    from: [],
    to: [],
    withdrawals: [],
  };
  @observable
  state: TransactionState;
  @observable
  confirmations: number;
  @observable
  slotNumber: number | null | undefined;
  @observable
  epochNumber: number | null | undefined;
  @observable
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
    Object.assign(this, data);
  }
}
