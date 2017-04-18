// @flow
import { Action } from './lib/actions';

// ======= TRANSACTIONS ACTIONS =======

export default class TransactionsActions {
  filterTransactions: Action<{ searchTerm: string }> = new Action();
  loadMoreTransactions: Action<any> = new Action();
}
