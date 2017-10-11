// @flow
import Action from '../lib/Action';

// ======= TRANSACTIONS ACTIONS =======

export default class TransactionsActions {
  filterTransactions: Action<{ searchTerm: string }> = new Action();
  loadMoreTransactions: Action<any> = new Action();
}
