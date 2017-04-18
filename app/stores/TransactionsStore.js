// @flow
import { observable, computed, action, extendObservable } from 'mobx';
import _ from 'lodash';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';
import WalletTransaction from '../domain/WalletTransaction';
import type { GetTransactionsResponse } from '../api';
import LocalizableError from '../i18n/LocalizableError';

export default class TransactionsStore extends Store {

  INITIAL_SEARCH_LIMIT = 1000;
  SEARCH_LIMIT_INCREASE = 500;
  SEARCH_SKIP = 0;
  RECENT_TRANSACTIONS_LIMIT = 5;

  @observable transactionsRequests: Array<{
    walletId: string,
    recentRequest: CachedRequest<GetTransactionsResponse, LocalizableError>,
    allRequest: CachedRequest<GetTransactionsResponse, LocalizableError>
  }> = [];

  @observable _searchOptionsForWallets = {};

  setup() {
    const actions = this.actions.transactions;
    actions.filterTransactions.listen(this._updateSearchTerm);
    actions.loadMoreTransactions.listen(this._increaseSearchLimit);
  }

  @action _updateSearchTerm = ({ searchTerm }: { searchTerm: string }) => {
    if (this.searchOptions != null) {
      this.searchOptions.searchTerm = searchTerm;
    }
  };

  @action _increaseSearchLimit = () => {
    if (this.searchOptions != null) {
      this.searchOptions.searchLimit += this.SEARCH_LIMIT_INCREASE;
    }
  };

  @computed get recentTransactionsRequest(): CachedRequest<GetTransactionsResponse, LocalizableError> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return new CachedRequest(this.api.getTransactions); // TODO: Do not return new request here
    return this._getTransactionsRecentRequest(wallet.id);
  }

  @computed get searchRequest(): CachedRequest<GetTransactionsResponse, LocalizableError> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return new CachedRequest(this.api.getTransactions); // TODO: Do not return new request here
    return this._getTransactionsAllRequest(wallet.id);
  }

  @computed get searchOptions(): ?TransactionSearchOptionsStruct {
    const wallet = this.stores.wallets.active;
    if (!wallet) return null;
    let options = this._searchOptionsForWallets[wallet.id];
    if (!options) {
      // Setup options for each requested wallet
      extendObservable(this._searchOptionsForWallets, {
        [wallet.id]: {
          searchTerm: '',
          searchLimit: this.INITIAL_SEARCH_LIMIT,
          searchSkip: this.SEARCH_SKIP
        }
      });
      options = this._searchOptionsForWallets[wallet.id];
    }
    return options;
  }

  @computed get filtered(): Array<WalletTransaction> {
    const wallet = this.stores.wallets.active;
    if (!wallet || !this.searchOptions) return [];
    const { searchTerm } = this.searchOptions;
    const request = this._getTransactionsAllRequest(wallet.id);
    if (searchTerm && request.result && request.result.transactions) {
      return request.result.transactions.filter(
        transaction => transaction.title.search(new RegExp(searchTerm, 'i')) !== -1
      );
    }
    return request.result ? request.result.transactions : [];
  }

  @computed get recent(): Array<WalletTransaction> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const result = this._getTransactionsRecentRequest(wallet.id).result;
    return result ? result.transactions.slice(0, this.RECENT_TRANSACTIONS_LIMIT) : [];
  }

  @computed get hasAnyFiltered(): bool {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const result = this._getTransactionsAllRequest(wallet.id).result;
    return result ? result.transactions.length > 0 : false;
  }

  @computed get hasAny(): bool {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const result = this._getTransactionsRecentRequest(wallet.id).result;
    return result ? result.transactions.length > 0 : false;
  }

  @computed get totalAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const result = this._getTransactionsAllRequest(wallet.id).result;
    return result ? result.transactions.length : 0;
  }

  @computed get totalUnconfirmedAmount(): BigNumber {
    let unconfirmedAmount = new BigNumber(0);
    const wallet = this.stores.wallets.active;
    if (!wallet) return unconfirmedAmount;
    const result = this._getTransactionsAllRequest(wallet.id).result;
    if (!result || !result.transactions) return unconfirmedAmount;

    for (const transaction of result.transactions) {
      // TODO: move this magic constant (required numberOfConfirmations) to config!
      if (transaction.numberOfConfirmations <= 6) {
        unconfirmedAmount = unconfirmedAmount.plus(transaction.amount.absoluteValue());
      }
    }
    return unconfirmedAmount;
  }

  @computed get totalFilteredAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const result = this._getTransactionsAllRequest(wallet.id).result;
    return result ? result.transactions.length : 0;
  }

  @action _refreshTransactionData = () => {
    if (this.stores.networkStatus.isConnected) {
      const allWallets = this.stores.wallets.all;
      for (const wallet of allWallets) {
        const recentRequest = this._getTransactionsRecentRequest(wallet.id);
        recentRequest.invalidate({ immediately: false });
        recentRequest.execute({
          walletId: wallet.id,
          limit: this.RECENT_TRANSACTIONS_LIMIT,
          skip: 0,
          searchTerm: '',
        });
        const allRequest = this._getTransactionsAllRequest(wallet.id);
        allRequest.invalidate({ immediately: false });
        allRequest.execute({
          walletId: wallet.id,
          limit: this.INITIAL_SEARCH_LIMIT,
          skip: 0,
          searchTerm: '',
        });
      }
    }
  };

  _getTransactionsRecentRequest = (walletId: string): CachedRequest<GetTransactionsResponse, LocalizableError> => {
    const foundRequest = _.find(this.transactionsRequests, { walletId });
    return foundRequest && foundRequest.recentRequest ? foundRequest.recentRequest : new CachedRequest(this.api.getTransactions);
  };

  _getTransactionsAllRequest = (walletId: string): CachedRequest<GetTransactionsResponse, LocalizableError> => {
    const foundRequest = _.find(this.transactionsRequests, { walletId });
    return foundRequest && foundRequest.allRequest ? foundRequest.allRequest : new CachedRequest(this.api.getTransactions);
  };

}

export type TransactionSearchOptionsStruct = {
  searchTerm: string,
  searchLimit: number,
  searchSkip: number,
};
