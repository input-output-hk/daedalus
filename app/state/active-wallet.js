// @flow
import Wallet from '../domain/Wallet';

export type activeWalletState = {
  wallet: ?Wallet,
  isLoading: bool,
  errorLoading: ?string,
  errorCreating: ?string,
  errorLoadingTransactions: ?string,
  errorSendingMoney: ?string
};

export default (): activeWalletState => ({
  wallet: null,
  isLoading: false,
  errorLoading: null,
  errorCreating: null,
  errorLoadingTransactions: null,
  errorSendingMoney: null
});
