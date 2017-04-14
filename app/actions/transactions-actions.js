// @flow
import { Action } from './lib/actions';

// ======= TRANSACTIONS ACTIONS =======

export type TransactionsActions = {
  filterTransactions: Action<{ searchTerm: string }>,
  loadMoreTransactions: Action<any>,
};

const transactionsActions: TransactionsActions = {
  filterTransactions: new Action(),
  loadMoreTransactions: new Action(),
};

export default transactionsActions;
