// @flow
import type { appState } from './index';
import Wallet from '../domain/Wallet';

export const INITIAL_WALLET_SEARCH_LIMIT = 10;

export type activeWalletState = {
  state: appState,
  wallet: ?Wallet,
  isLoadingTransactions: boolean,
  transactionsSearchTerm: string,
  transactionsSearchLimit: number,
  totalAvailableTransactions: number,
  isLoading: boolean,
  errorLoading: ?string,
  errorCreating: ?string,
  errorLoadingTransactions: ?string,
  errorSendingMoney: ?string
};

export default (state: appState): activeWalletState => ({
  state,
  wallet: null,
  isLoadingTransactions: false,
  transactionsSearchTerm: '',
  transactionsSearchLimit: INITIAL_WALLET_SEARCH_LIMIT,
  totalAvailableTransactions: 0,
  isLoading: false,
  errorLoading: null,
  errorCreating: null,
  errorLoadingTransactions: null,
  errorSendingMoney: null
});
