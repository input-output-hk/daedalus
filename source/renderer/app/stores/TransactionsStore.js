// @flow
import {
  observable,
  computed,
  action,
  extendObservable,
  runInAction,
} from 'mobx';
import { find } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { WalletTransaction } from '../domains/WalletTransaction';
import type {
  DeleteTransactionRequest,
  GetTransactionsResponse,
} from '../api/transactions/types';
import { isValidAmountInLovelaces } from '../utils/validations';
// import { WalletSyncStateStatuses } from '../domains/Wallet';

/* eslint-disable consistent-return */

export type TransactionSearchOptionsStruct = {
  searchTerm: string,
  searchLimit: number,
  searchSkip: number,
};

type TransactionFeeRequest = {
  walletId: string,
  address: string,
  amount: number,
};

export default class TransactionsStore extends Store {
  INITIAL_SEARCH_LIMIT = null; // 'null' value stands for 'load all'
  SEARCH_LIMIT_INCREASE = 500;
  SEARCH_SKIP = 0;
  RECENT_TRANSACTIONS_LIMIT = 50;

  @observable transactionsRequests: Array<{
    walletId: string,
    isLegacy: boolean,
    recentRequest: Request<GetTransactionsResponse>,
    allRequest: Request<GetTransactionsResponse>,
  }> = [];
  @observable
  deleteTransactionRequest: Request<DeleteTransactionRequest> = new Request(
    this.api.ada.deleteTransaction
  );

  @observable _searchOptionsForWallets = {};

  setup() {
    // const actions = this.actions.transactions;
    // actions.filterTransactions.listen(this._updateSearchTerm);
    // actions.loadMoreTransactions.listen(this._increaseSearchLimit);
    this.registerReactions([this._ensureSearchOptionsForActiveWallet]);
  }

  @computed get recentTransactionsRequest(): Request<GetTransactionsResponse> {
    const wallet = this.stores.wallets.active;
    // TODO: Do not return new request here
    if (!wallet) return new Request(this.api.ada.getTransactions);
    return this._getTransactionsRecentRequest(wallet.id);
  }

  @computed get searchRequest(): Request<GetTransactionsResponse> {
    const wallet = this.stores.wallets.active;
    // TODO: Do not return new request here
    if (!wallet) return new Request(this.api.ada.getTransactions);
    return this._getTransactionsAllRequest(wallet.id);
  }

  @computed get searchOptions(): ?TransactionSearchOptionsStruct {
    const wallet = this.stores.wallets.active;
    if (!wallet) return null;
    return this._searchOptionsForWallets[wallet.id];
  }

  @computed get filtered(): Array<WalletTransaction> {
    const wallet = this.stores.wallets.active;
    if (!wallet || !this.searchOptions) return [];
    const { searchTerm } = this.searchOptions;
    const request = this._getTransactionsAllRequest(wallet.id);
    if (searchTerm && request.result && request.result.transactions) {
      return request.result.transactions.filter(
        transaction =>
          transaction.title.search(new RegExp(searchTerm, 'i')) !== -1
      );
    }
    return request.result ? request.result.transactions : [];
  }

  @computed get recent(): Array<WalletTransaction> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const results = this._getTransactionsRecentRequest(wallet.id).result;
    return results ? results.transactions : [];
  }

  @computed get hasAnyFiltered(): boolean {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const results = this._getTransactionsAllRequest(wallet.id).result;
    return results ? results.transactions.length > 0 : false;
  }

  @computed get hasAny(): boolean {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const results = this._getTransactionsRecentRequest(wallet.id).result;
    return results ? results.total > 0 : false;
  }

  @computed get totalAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const results = this._getTransactionsAllRequest(wallet.id).result;
    return results ? results.total : 0;
  }

  @computed get totalFilteredAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const results = this._getTransactionsAllRequest(wallet.id).result;
    return results ? results.transactions.length : 0;
  }

  @computed get pendingTransactionsCount(): number {
    return this.recent.filter(({ state }) => state === 'pending').length;
  }

  @action _refreshTransactionData = async () => {
    if (this.stores.networkStatus.isConnected) {
      const { all: wallets } = this.stores.wallets;
      for (const wallet of wallets) {
        const recentRequest = this._getTransactionsRecentRequest(wallet.id);
        recentRequest.execute({
          walletId: wallet.id,
          order: 'descending',
          fromDate: null,
          toDate: null,
          isLegacy: wallet.isLegacy,
          // @API TODO - Params "pending" for V2
          // limit: this.RECENT_TRANSACTIONS_LIMIT,
          // skip: 0,
          // searchTerm: '',
          // isFirstLoad: !recentRequest.wasExecuted,
          // isRestoreActive,
          // isRestoreCompleted,
          // cachedTransactions: get(recentRequest, 'result.transactions', []),
        });
        const allRequest = this._getTransactionsAllRequest(wallet.id);
        allRequest.execute({
          walletId: wallet.id,
          order: 'descending',
          fromDate: null,
          toDate: null,
          isLegacy: wallet.isLegacy,
          // @API TODO - Params "pending" for V2
          // limit: this.INITIAL_SEARCH_LIMIT,
          // skip: 0,
          // searchTerm: '',
          // isFirstLoad: !allRequest.wasExecuted,
          // isRestoreActive,
          // isRestoreCompleted,
          // cachedTransactions: get(allRequest, 'result.transactions', []),
        });
      }
    }
  };

  calculateTransactionFee = async (
    transactionFeeRequest: TransactionFeeRequest
  ) => {
    const { walletId } = transactionFeeRequest;
    const wallet = this.stores.wallets.getWalletById(walletId);

    if (!wallet) {
      throw new Error(
        'Active wallet required before calculating transaction fees.'
      );
    }

    return this.api.ada.calculateTransactionFee({
      ...transactionFeeRequest,
      walletBalance: wallet.amount,
      availableBalance: wallet.availableAmount,
    });
  };

  deletePendingTransaction = async ({
    walletId,
    transactionId,
  }: {
    walletId: string,
    transactionId: string,
  }) => {
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet) {
      throw new Error(
        'Active wallet required before deleting a pending transaction.'
      );
    }
    const { isLegacy } = wallet;
    await this.deleteTransactionRequest.execute({
      walletId,
      transactionId,
      isLegacy,
    });
    this.stores.wallets.refreshWalletsData();
  };

  validateAmount = (amountInLovelaces: string): Promise<boolean> =>
    Promise.resolve(isValidAmountInLovelaces(amountInLovelaces));

  // ======================= PRIVATE ========================== //

  @action _updateSearchTerm = ({ searchTerm }: { searchTerm: string }) => {
    if (this.searchOptions != null) {
      this.searchOptions.searchTerm = searchTerm;
    }
  };

  _getTransactionsRecentRequest = (
    walletId: string
  ): Request<GetTransactionsResponse> => {
    const foundRequest = find(this.transactionsRequests, { walletId });

    if (foundRequest && foundRequest.recentRequest)
      return foundRequest.recentRequest;
    return new Request(this.api.ada.getTransactions);
  };

  _getTransactionsAllRequest = (
    walletId: string
  ): Request<GetTransactionsResponse> => {
    const foundRequest = find(this.transactionsRequests, { walletId });

    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new Request(this.api.ada.getTransactions);
  };

  // ======================= REACTIONS ========================== //

  /**
   * Reaction that makes sure that we have some default (empty)
   * search options for the active wallet.
   * @private
   */
  _ensureSearchOptionsForActiveWallet = () => {
    const wallet = this.stores.wallets.active;
    if (!wallet) return;
    const options = this._searchOptionsForWallets[wallet.id];
    if (!options) {
      // Setup options for active wallet
      runInAction('setSearchOptionsForActiveWallet', () => {
        extendObservable(this._searchOptionsForWallets, {
          [wallet.id]: {
            searchTerm: '',
            searchLimit: this.INITIAL_SEARCH_LIMIT,
            searchSkip: this.SEARCH_SKIP,
          },
        });
      });
    }
  };
}
