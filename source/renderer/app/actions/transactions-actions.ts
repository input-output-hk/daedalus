import Action from './lib/Action';
import type { TransactionFilterOptionsType } from '../stores/TransactionsStore'; // ======= TRANSACTIONS ACTIONS =======

export default class TransactionsActions {
  filterTransactions: Action<TransactionFilterOptionsType> = new Action();
  loadMoreTransactions: Action<any> = new Action();
  requestCSVFile: Action<any> = new Action();
  requestCSVFileSuccess: Action<any> = new Action();
}
