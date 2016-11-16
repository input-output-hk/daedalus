// @flow
import Wallet from '../domain/Wallet';

export type activeWalletState = {
  wallet: ?Wallet,
  transactionsSearchTerm: string,
  isLoading: bool,
  errorLoading: ?string,
  errorCreating: ?string,
  errorLoadingTransactions: ?string,
  errorSendingMoney: ?string
};

export default (): activeWalletState => ({
  wallet: null,
  transactionsSearchTerm: '',
  isLoading: false,
  errorLoading: null,
  errorCreating: null,
  errorLoadingTransactions: null,
  errorSendingMoney: null
});
