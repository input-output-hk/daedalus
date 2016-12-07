// @flow
import { action } from 'mobx';
import Wallet from '../domain/Wallet';

export const INITIAL_WALLET_SEARCH_LIMIT = 10;

export type activeWalletState = {
  wallet: ?Wallet,
  isLoadingTransactions: boolean,
  transactionsSearchTerm: string,
  transactionsSearchLimit: number,
  totalAvailableTransactions: number,
  isLoading: boolean,
  hasAnyTransactions: boolean,
  errorLoading: ?string,
  errorCreating: ?string,
  errorLoadingTransactions: ?string,
  errorSendingMoney: ?string,
  reset: () => void
};

const defaultValues = {
  wallet: null,
  isLoadingTransactions: false,
  transactionsSearchTerm: '',
  transactionsSearchLimit: INITIAL_WALLET_SEARCH_LIMIT,
  totalAvailableTransactions: 0,
  isLoading: false,
  hasAnyTransactions: false,
  errorLoading: null,
  errorCreating: null,
  errorLoadingTransactions: null,
  errorSendingMoney: null,
};

const state = {};

export default (): activeWalletState => (Object.assign(state, defaultValues, {
  reset: action(() => Object.assign(state, defaultValues))
}));
