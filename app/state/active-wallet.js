// @flow
import Wallet from '../domain/Wallet';

export type activeWalletState = {
  wallet: ?Wallet,
  transactionsSearchTerm: string,
  transactionsSearchLimit: number,
  isLoading: bool,
  errorLoading: ?string,
  errorCreating: ?string,
  errorLoadingTransactions: ?string,
  errorSendingMoney: ?string
};

export default (): activeWalletState => ({
  wallet: null,
  transactionsSearchTerm: '',
  transactionsSearchLimit: 10,
  isLoading: false,
  errorLoading: null,
  errorCreating: null,
  errorLoadingTransactions: null,
  errorSendingMoney: null
});
