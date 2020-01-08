// @flow
import Action from './lib/Action';

// ======= TRANSACTIONS ACTIONS =======

export default class TransactionsActions {
  filterTransactions: Action<{
    searchTerm?: string,
    searchLimit?: number,
    searchSkip?: number,
    fromDate?: string,
    toDate?: string,
    fromAmount?: number,
    toAmount?: number,
    incomingChecked?: boolean,
    outgoingChecked?: boolean,
  }> = new Action();
  loadMoreTransactions: Action<any> = new Action();
}
