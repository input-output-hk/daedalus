// @flow
import Wallet from '../domain/Wallet';

export type walletsState = {
  activeWallet: ?Wallet,
  isLoading: bool,
  errorLoading: ?string,
  errorCreating: ?string,
  errorLoadingTransactions: ?string,
  errorSendingMoney: ?string
};

export default (): walletsState => ({
  activeWallet: null,
  isLoading: false,
  errorLoading: null,
  errorCreating: null,
  errorLoadingTransactions: null,
  errorSendingMoney: null
});
