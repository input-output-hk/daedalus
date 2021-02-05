// @flow
import { observable } from 'mobx';
import BigNumber from 'bignumber.js';
import type {
  TransactionAddresses,
  TransactionType,
  TransactionDepth,
  TransactionState,
  TransactionWithdrawalType,
} from '../api/transactions/types';
import type { WalletAssetItems } from '../api/assets/types';
import { WalletUnits } from './Wallet';

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
  @observable assets: {
    input: ?WalletAssetItems,
    output: ?WalletAssetItems,
  };
  @observable fee: {
    quantity: number,
    unit: WalletUnits.LOVELACE,
  };
  @observable date: ?Date;
  @observable description: string = '';
  @observable addresses: TransactionAddresses = {
    from: [],
    to: [],
    withdrawals: [],
    currencies: [],
  };
  @observable state: TransactionState;
  @observable depth: TransactionDepth;
  @observable slotNumber: ?number;
  @observable epochNumber: ?number;

  constructor(data: {
    id: string,
    type: TransactionType,
    title: string,
    amount: BigNumber,
    fee: {
      quantity: number,
      unit: WalletUnits.LOVELACE,
    };
    assets?: {
      input: ?WalletAssetItems,
      output: ?WalletAssetItems,
    },
    date: ?Date,
    description: string,
    addresses: TransactionAddresses,
    state: TransactionState,
    depth: TransactionDepth,
    slotNumber: ?number,
    epochNumber: ?number,
  }) {
    Object.assign(this, data);
  }
}
