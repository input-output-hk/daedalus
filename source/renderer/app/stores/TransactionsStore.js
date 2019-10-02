// @flow
import {
  observable,
  computed,
  action,
  extendObservable,
  runInAction,
} from 'mobx';
import BigNumber from 'bignumber.js';
import { find } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  WalletTransaction,
  TransactionTypes,
} from '../domains/WalletTransaction';
import type { GetTransactionsResponse } from '../api/transactions/types';
import type { UnconfirmedAmount } from '../types/unconfirmedAmountType';
import { isValidAmountInLovelaces } from '../utils/validations';
import { TX_UNCONFIRMED_THRESHOLD } from '../config/numbersConfig';
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
    recentRequest: Request<GetTransactionsResponse>,
    allRequest: Request<GetTransactionsResponse>,
  }> = [];

  @observable _searchOptionsForWallets = {};

  @observable
  unconfirmedAmount: UnconfirmedAmount = this._getEmptyUnconfirmedAmount();

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

  @action _refreshTransactionData = async (restoredWalletId: ?string) => {
    if (this.stores.networkStatus.isConnected) {
      const allWallets = this.stores.wallets.all;
      for (const wallet of allWallets) {
        // @API TODO - Params "pending" for V2
        // const isRestoreActive =
        //   get(wallet, ['syncState', 'status'], '') ===
        //   WalletSyncStateStatuses.RESTORING;
        const isRestoreCompleted = restoredWalletId === wallet.id;
        const recentRequest = this._getTransactionsRecentRequest(wallet.id);
        if (isRestoreCompleted && recentRequest.isExecuting) {
          // We need to make sure to run recentRequest if the restoration has just completed
          // as otherwise some transactions could be lost!
          await recentRequest;
        }
        recentRequest.execute({
          walletId: wallet.id,
          order: 'descending',
          fromDate: null,
          toDate: null,
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
        if (isRestoreCompleted && allRequest.isExecuting) {
          // We need to make sure to run allRequest if the restoration has just completed
          // as otherwise some transactions could be lost!
          await allRequest;
        }
        allRequest.execute({
          walletId: wallet.id,
          order: 'descending',
          fromDate: null,
          toDate: null,
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
    });
  };

  validateAmount = (amountInLovelaces: string): Promise<boolean> =>
    Promise.resolve(isValidAmountInLovelaces(amountInLovelaces));

  // ======================= PRIVATE ========================== //

  @action _updateSearchTerm = ({ searchTerm }: { searchTerm: string }) => {
    if (this.searchOptions != null) {
      this.searchOptions.searchTerm = searchTerm;
    }
  };

  _getEmptyUnconfirmedAmount(): UnconfirmedAmount {
    return {
      total: new BigNumber(0),
      incoming: new BigNumber(0),
      outgoing: new BigNumber(0),
    };
  }

  _setUnconfirmedAmount(amount: UnconfirmedAmount) {
    Object.assign(this.unconfirmedAmount, amount);
  }

  _resetUnconfirmedAmount() {
    this._setUnconfirmedAmount(this._getEmptyUnconfirmedAmount());
  }

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
   * Reaction that recomputes unconfirmed amounts for the active wallet
   * and then updates the observable struct with the new props.
   * @private
   */
  @action _calculateUnconfirmedAmount() {
    // Reset when no active wallet
    const wallet = this.stores.wallets.active;
    if (!wallet) return this._resetUnconfirmedAmount();
    // Reset when no transactions
    const results = this._getTransactionsAllRequest(wallet.id).result;
    if (!results || !results.transactions)
      return this._resetUnconfirmedAmount();

    // We have some results, lets compute and update
    const unconfirmedAmount = this._getEmptyUnconfirmedAmount();
    for (const transaction of results.transactions) {
      if (transaction.numberOfConfirmations <= TX_UNCONFIRMED_THRESHOLD) {
        unconfirmedAmount.total = unconfirmedAmount.total.plus(
          transaction.amount.absoluteValue()
        );
        if (transaction.type === TransactionTypes.EXPEND) {
          unconfirmedAmount.outgoing = unconfirmedAmount.outgoing.plus(
            transaction.amount.absoluteValue()
          );
        }
        if (transaction.type === TransactionTypes.INCOME) {
          unconfirmedAmount.incoming = unconfirmedAmount.incoming.plus(
            transaction.amount.absoluteValue()
          );
        }
      }
    }
    this._setUnconfirmedAmount(unconfirmedAmount);
  }
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
