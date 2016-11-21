// @flow
import type { appState } from './index';
import Wallet from '../domain/Wallet';

export const INITIAL_WALLET_SEARCH_LIMIT = 10;

export type activeWalletState = {
  state: appState,
  wallet: ?Wallet,
  isLoadingTransactions: boolean,
  isInitiallyLoadingTransactions: boolean,
  transactionsSearchTerm: string,
  transactionsSearchLimit: number,
  totalAvailableTransactions: number,
  isLoading: boolean,
  hasAnyTransactions: boolean,
  errorLoading: ?string,
  errorCreating: ?string,
  errorLoadingTransactions: ?string,
  errorSendingMoney: ?string
};

export default (state: appState): activeWalletState => ({
  state,
  wallet: null,
  isLoadingTransactions: false,
  isInitiallyLoadingTransactions: false,
  transactionsSearchTerm: '',
  transactionsSearchLimit: INITIAL_WALLET_SEARCH_LIMIT,
  totalAvailableTransactions: 0,
  isLoading: false,
  hasAnyTransactions: false,
  errorLoading: null,
  errorCreating: null,
  errorLoadingTransactions: null,
  errorSendingMoney: null
});
