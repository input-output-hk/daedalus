import moment from 'moment';
import { WalletTransaction } from '../../../domains/WalletTransaction';

export class TransactionsGroup {
  date: moment.Moment;
  transactions: WalletTransaction[];

  constructor(props: {
    date: moment.Moment;
    transactions: WalletTransaction[];
  }) {
    Object.assign(this, props);
  }
}
export class TransactionInfo {
  tx: WalletTransaction;
  isLastInGroup: boolean;
  isFirstInGroup: boolean;

  constructor(props: {
    tx: WalletTransaction;
    isLastInGroup: boolean;
    isFirstInGroup: boolean;
  }) {
    Object.assign(this, props);
  }
}
export type Row = TransactionsGroup | TransactionInfo;
