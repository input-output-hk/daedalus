// @flow
import {
  observable,
  computed,
  action,
  extendObservable,
  runInAction,
} from 'mobx';
import { find, get } from 'lodash';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { WalletTransaction } from '../domains/WalletTransaction';
import type {
  DeleteTransactionRequest,
  GetTransactionsResponse,
  GetWithdrawalsResponse,
} from '../api/transactions/types';
import { isValidAmountInLovelaces } from '../utils/validations';
import {
  generateFilterOptions,
  isTransactionInFilterRange,
} from '../utils/transaction';

const INITIAL_SEARCH_LIMIT = null; // 'null' value stands for 'load all'
const SEARCH_LIMIT_INCREASE = 500; // eslint-disable-line
const SEARCH_SKIP = 0;
const RECENT_TRANSACTIONS_LIMIT = 50; // eslint-disable-line

export type DateRangeType =
  | ''
  | 'last7Days'
  | 'last30Days'
  | 'last90Days'
  | 'thisYear'
  | 'custom';

export const DateRangeTypes = {
  LAST_7_DAYS: 'last7Days',
  LAST_30_DAYS: 'last30Days',
  LAST_90_DAYS: 'last90Days',
  THIS_YEAR: 'thisYear',
  CUSTOM: 'custom',
};

export type TransactionFilterOptionsType = {
  searchTerm?: string,
  searchLimit?: ?number,
  searchSkip?: ?number,
  dateRange?: DateRangeType,
  fromDate?: string,
  toDate?: string,
  fromAmount?: string,
  toAmount?: string,
  incomingChecked?: boolean,
  outgoingChecked?: boolean,
};

export const emptyTransactionFilterOptions = {
  searchTerm: '',
  searchLimit: INITIAL_SEARCH_LIMIT,
  searchSkip: SEARCH_SKIP,
  dateRange: '',
  fromDate: '',
  toDate: '',
  fromAmount: '',
  toAmount: '',
  incomingChecked: true,
  outgoingChecked: true,
};

type TransactionFeeRequest = {
  walletId: string,
  address: string,
  amount: number,
};

export default class TransactionsStore extends Store {
  @observable transactionsRequests: Array<{
    walletId: string,
    isLegacy: boolean,
    recentRequest: Request<GetTransactionsResponse>,
    allRequest: Request<GetTransactionsResponse>,
    withdrawalsRequest: Request<GetWithdrawalsResponse>,
  }> = [];

  @observable
  deleteTransactionRequest: Request<DeleteTransactionRequest> = new Request(
    this.api.ada.deleteTransaction
  );

  @observable _filterOptionsForWallets = {};

  setup() {
    const {
      transactions: transactionActions,
      networkStatus: networkStatusActions,
    } = this.actions;
    transactionActions.filterTransactions.listen(this._updateFilterOptions);
    // transactionActions.loadMoreTransactions.listen(this._increaseSearchLimit);
    networkStatusActions.restartNode.listen(this._clearFilterOptions);
    this.registerReactions([this._ensureFilterOptionsForActiveWallet]);
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

  @computed get filterOptions(): ?TransactionFilterOptionsType {
    const wallet = this.stores.wallets.active;
    if (!wallet) return null;
    return this._filterOptionsForWallets[wallet.id];
  }

  @computed get withdrawals(): { [string]: BigNumber } {
    const withdrawals = {};
    const { allWallets: wallets } = this.stores.wallets;
    for (const wallet of wallets) {
      const { id: walletId } = wallet;
      const request = this._getWithdrawalsRequest(walletId);
      withdrawals[walletId] =
        get(request, 'result.withdrawals') || new BigNumber(0);
    }
    return withdrawals;
  }

  @computed get all(): Array<WalletTransaction> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const request = this._getTransactionsAllRequest(wallet.id);

    if (!request.result) {
      return [];
    }

    return request.result.transactions || [];
  }

  @computed get allFiltered(): Array<WalletTransaction> {
    return this.all.filter(transaction =>
      isTransactionInFilterRange(this.filterOptions, transaction)
    );
  }

  @computed get defaultFilterOptions(): TransactionFilterOptionsType {
    return generateFilterOptions(this.all);
  }

  @computed get populatedFilterOptions(): TransactionFilterOptionsType {
    return this.filterOptions || emptyTransactionFilterOptions;
  }

  @computed get recent(): Array<WalletTransaction> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const results = this._getTransactionsRecentRequest(wallet.id).result;
    return results ? results.transactions : [];
  }

  @computed get recentFiltered(): Array<WalletTransaction> {
    return this.recent.filter(transaction =>
      isTransactionInFilterRange(this.filterOptions, transaction)
    );
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
        if (!wallet.isLegacy) {
          const withdrawalsRequest = this._getWithdrawalsRequest(wallet.id);
          withdrawalsRequest.execute({ walletId: wallet.id });
        }
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
      isLegacy: wallet.isLegacy,
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

  @action _updateFilterOptions = (
    filterOptions: TransactionFilterOptionsType
  ) => {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const currentFilterOptions = this._filterOptionsForWallets[wallet.id];
    this._filterOptionsForWallets[wallet.id] = {
      ...currentFilterOptions,
      ...filterOptions,
    };
    return true;
  };

  @action _clearFilterOptions = () => {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    this._filterOptionsForWallets[wallet.id] = {
      ...emptyTransactionFilterOptions,
    };
    return true;
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

  _getWithdrawalsRequest = (
    walletId: string
  ): Request<GetWithdrawalsResponse> => {
    const foundRequest = find(this.transactionsRequests, { walletId });
    if (foundRequest && foundRequest.withdrawalsRequest)
      return foundRequest.withdrawalsRequest;
    return new Request(this.api.ada.getWithdrawals);
  };

  // ======================= REACTIONS ========================== //

  /**
   * Reaction that makes sure that we have some default (empty)
   * search options for the active wallet.
   * @private
   */
  _ensureFilterOptionsForActiveWallet = () => {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const options = this._filterOptionsForWallets[wallet.id];
    if (!options) {
      // Setup options for active wallet
      runInAction('setFilterOptionsForActiveWallet', () => {
        extendObservable(this._filterOptionsForWallets, {
          [wallet.id]: emptyTransactionFilterOptions,
        });
      });
    }
    return true;
  };
}
