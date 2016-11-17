// @flow
import Wallet from '../domain/Wallet';

export type activeWalletState = {
  wallet: ?Wallet,
  isLoadingTransactions: boolean,
  transactionsSearchTerm: string,
  transactionsSearchLimit: number,
  totalAvailableTransactions: number,
  isLoading: bool,
  errorLoading: ?string,
  errorCreating: ?string,
  errorLoadingTransactions: ?string,
  errorSendingMoney: ?string
};

export default (): activeWalletState => ({
  wallet: null,
  isLoadingTransactions: false,
  transactionsSearchTerm: '',
  transactionsSearchLimit: 10,
  totalAvailableTransactions: 0,
  isLoading: false,
  errorLoading: null,
  errorCreating: null,
  errorLoadingTransactions: null,
  errorSendingMoney: null
});
