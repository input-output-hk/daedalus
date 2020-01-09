// @flow
import Action from './lib/Action';
import type { TransactionFilterOptionsStruct } from '../stores/TransactionsStore';

// ======= TRANSACTIONS ACTIONS =======

export default class TransactionsActions {
  filterTransactions: Action<TransactionFilterOptionsStruct> = new Action();
  loadMoreTransactions: Action<any> = new Action();
}
