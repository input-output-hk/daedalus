// @flow
import { observable, computed, action, extendObservable } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import Request from './lib/Request';
import CachedRequest from './lib/CachedRequest';
import WalletTransaction from '../domain/WalletTransaction';
import environment from '../environment';

export default class TransactionsStore extends Store {

  INITIAL_SEARCH_LIMIT = 10;
  SEARCH_LIMIT_INCREASE = 10;
  SEARCH_SKIP = 0;
  RECENT_TRANSACTIONS_LIMIT = 5;
  TRANSACTION_REFRESH_INTERVAL = 5000;

  @observable transactionCache: [{ walletId: string, transactions: [WalletTransaction] }] = [];
  @observable recentTransactionRequests: [{ walletId: string, recentRequest: [Request] }] = [];

  @observable searchRequest = new CachedRequest(this.api, 'getTransactions');
  @observable recentRequest = new Request(this.api, 'getTransactions');
  @observable transactionsRequest = new Request(this.api, 'getTransactions');
  @observable _searchOptionsForWallets = {};

  constructor(...args) {
    super(...args);
    this.actions.filterTransactions.listen(this._updateSearchTerm);
    this.actions.loadMoreTransactions.listen(this._increaseSearchLimit);
    if (environment.CARDANO_API) {
      setInterval(this._refreshTransactionData, this.TRANSACTION_REFRESH_INTERVAL);
      setInterval(this._refreshRecentTransactionsData, this.TRANSACTION_REFRESH_INTERVAL);
    }
  }

  @action _updateSearchTerm = ({ searchTerm }) => { this.searchOptions.searchTerm = searchTerm; };
  @action _increaseSearchLimit = () => { this.searchOptions.searchLimit += this.SEARCH_LIMIT_INCREASE; };

  @computed get recentTransactionsRequest() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return new Request(this.api, 'getTransactions'); // TODO: Do not return new request here
    console.log('this._getTransactionsRecentRequest(wallet.id)', this._getTransactionsRecentRequest(wallet.id));
    return this._getTransactionsRecentRequest(wallet.id);
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
      // Reset the search results when a wallet is queried for the first time
      this.searchRequest.patch(result => null);
    }
    return options;
  }

  @computed get filtered() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const { searchTerm, searchLimit, searchSkip } = this.searchOptions;
    const { result } = this.searchRequest.execute({
      walletId: wallet.id,
      limit: searchLimit,
      skip: searchSkip,
      searchTerm,
    });
    return result ? result.transactions : [];
  }

  @computed get recent() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const result = this._getTransactionsRecentRequest(wallet.id).result;
    return result ? result.transactions : [];
  }

  @computed get hasAnyFiltered() {
    const { result } = this.searchRequest;
    return result ? result.total > 0 : false;
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
    return this._getTransactionsForWallet(wallet.id).length;
  }

  @computed get totalUnconfirmedAmount() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    let unconfirmedAmount = 0;
    for (let transaction of this._getTransactionsForWallet(wallet.id)) {
      if (transaction.numberOfConfirmations <= 6) {
        unconfirmedAmount += transaction.amount < 0 ? (-1 * transaction.amount) : transaction.amount;
      }
    }
    return unconfirmedAmount;
  }

  @computed get totalFilteredAvailable() {
    const { result } = this.searchRequest;
    return result ? result.total : 0;
  }

  @action _refreshRecentTransactionsData = () => {
    if (this.stores.networkStatus.isCardanoConnected) {
      const allWallets = this.stores.wallets.all;
      for (let wallet of allWallets) {
        const request = this._getTransactionsRecentRequest(wallet.id);
        request.execute({
          walletId: wallet.id,
          limit: this.RECENT_TRANSACTIONS_LIMIT,
          skip: 0,
          searchTerm: '',
        });
      }
    }
  };

  @action _refreshTransactionData = () => {
    if (this.stores.networkStatus.isCardanoConnected) {
      const allWallets = this.stores.wallets.all;
      for (let wallet of allWallets) {
        this.transactionsRequest.execute({
          walletId: wallet.id,
          limit: 1000,
          skip: this._getTransactionsForWallet(wallet.id).length,
          searchTerm: '',
        });
        const result = this.transactionsRequest.result;
        this._setTransactionsForWallet(wallet.id, result ? result.transactions : []);
      }
    }
  };

  _getTransactionsRecentRequest = (walletId: string) => {
    const foundRequest = _.find(this.recentTransactionRequests, { walletId });
    console.log('foundRequest', foundRequest);
    return foundRequest && foundRequest.recentRequest ? foundRequest.recentRequest : new Request(this.api, 'getTransactions');
  };

  _getTransactionsForWallet = (walletId: string) => {
    const walletTransactions = _.find(this.transactionCache, { walletId });
    return walletTransactions && walletTransactions.transactions ? walletTransactions.transactions : [];
  };

  @action _setTransactionsForWallet = (walletId: string, transactions: Array<WalletTransaction>) => {
    // const currentTransactions = _.find(this.transactionCache, { walletId }).transactions;
    // for (let newTransaction of transactions) {
    //   let currentTransaction = _.find(currentTransactions, { id: newTransaction.id});
    //   if (currentTransaction) {
    //     currentTransaction = newTransaction;
    //   } else {
    //     currentTransactions.push(newTransaction);
    //   }
    // }
  };

}
