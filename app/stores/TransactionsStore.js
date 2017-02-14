// @flow
import { observable, computed, action, extendObservable } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';

export default class TransactionsStore extends Store {

  INITIAL_SEARCH_LIMIT = 1000;
  SEARCH_LIMIT_INCREASE = 500;
  SEARCH_SKIP = 0;
  RECENT_TRANSACTIONS_LIMIT = 5;

  @observable transactionsRequests: [{
    walletId: string,
    recentRequest: [CachedRequest],
    allRequest: [CachedRequest]
  }] = [];

  @observable _searchOptionsForWallets = {};

  constructor(...args) {
    super(...args);
    this.actions.filterTransactions.listen(this._updateSearchTerm);
    this.actions.loadMoreTransactions.listen(this._increaseSearchLimit);
  }

  @action _updateSearchTerm = ({ searchTerm }) => { this.searchOptions.searchTerm = searchTerm; };
  @action _increaseSearchLimit = () => { this.searchOptions.searchLimit += this.SEARCH_LIMIT_INCREASE; };

  @computed get recentTransactionsRequest() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return new CachedRequest(this.api, 'getTransactions'); // TODO: Do not return new request here
    return this._getTransactionsRecentRequest(wallet.id);
  }

  @computed get searchRequest() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return new CachedRequest(this.api, 'getTransactions'); // TODO: Do not return new request here
    return this._getTransactionsAllRequest(wallet.id);
  }

  @computed get searchOptions() {
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

  @computed get filtered() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const { searchTerm } = this.searchOptions;
    const request = this._getTransactionsAllRequest(wallet.id);
    if (searchTerm && request.result && request.result.transactions) {
      return request.result.transactions.filter(
        transaction => transaction.title.search(new RegExp(searchTerm, "i")) !== -1
      );
    } else {
      return request.result ? request.result.transactions : [];
    }
  }

  @computed get recent() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const result = this._getTransactionsRecentRequest(wallet.id).result;
    return result ? result.transactions : [];
  }

  @computed get hasAnyFiltered() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const result = this._getTransactionsAllRequest(wallet.id).result;
    return result ? result.transactions.length > 0 : false;
  }

  @computed get hasAny() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const result = this._getTransactionsRecentRequest(wallet.id).result;
    return result ? result.transactions.length > 0 : false;
  }

  @computed get totalAvailable() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const result = this._getTransactionsAllRequest(wallet.id).result;
    return result ? result.transactions.length : 0;
  }

  @computed get totalUnconfirmedAmount() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const result = this._getTransactionsAllRequest(wallet.id).result;
    if (!result || !result.transactions) return 0;
    let unconfirmedAmount = 0;
    for (let transaction of result.transactions) {
      if (transaction.numberOfConfirmations <= 6) {
        unconfirmedAmount += transaction.amount < 0 ? (-1 * transaction.amount) : transaction.amount;
      }
    }
    return unconfirmedAmount;
  }

  @computed get totalFilteredAvailable() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const result = this._getTransactionsAllRequest(wallet.id).result;
    return result ? result.transactions.length : 0;
  }

  @action _refreshTransactionData = () => {
    if (this.stores.networkStatus.isCardanoConnected) {
      const allWallets = this.stores.wallets.all;
      for (let wallet of allWallets) {
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

  _getTransactionsRecentRequest = (walletId: string) => {
    const foundRequest = _.find(this.transactionsRequests, { walletId });
    return foundRequest && foundRequest.recentRequest ? foundRequest.recentRequest : new CachedRequest(this.api, 'getTransactions');
  };

  _getTransactionsAllRequest = (walletId: string) => {
    const foundRequest = _.find(this.transactionsRequests, { walletId });
    return foundRequest && foundRequest.allRequest ? foundRequest.allRequest : new CachedRequest(this.api, 'getTransactions');
  };

}
