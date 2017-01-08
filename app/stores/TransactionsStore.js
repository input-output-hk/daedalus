// @flow
import { observable, computed, action, extendObservable } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';

export default class TransactionsStore extends Store {

  INITIAL_SEARCH_LIMIT = 10;
  SEARCH_LIMIT_INCREASE = 10;

  @observable searchRequest = new CachedRequest(this.api, 'getTransactions');
  @observable _searchOptionsForWallets = {};

  constructor(...args) {
    super(...args);
    this.actions.filterTransactions.listen(this._updateSearchTerm);
    this.actions.loadMoreTransactions.listen(this._increaseSearchLimit);
  }

  @action _updateSearchTerm = ({ searchTerm }) => this.searchOptions.searchTerm = searchTerm;
  @action _increaseSearchLimit = () => this.searchOptions.searchLimit += this.SEARCH_LIMIT_INCREASE;

  @computed get searchOptions() {
    const wallet = this.stores.wallets.active;
    let options = this._searchOptionsForWallets[wallet.id];
    if (!options) {
      // Setup options for each requested wallet
      extendObservable(this._searchOptionsForWallets, {
        [wallet.id]: {
          searchTerm: '',
          searchLimit: this.INITIAL_SEARCH_LIMIT,
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
    const { searchTerm, searchLimit } = this.searchOptions;
    const { result } = this.searchRequest.execute({
      walletId: wallet.id,
      limit: searchLimit,
      searchTerm,
    });
    return result ? result.transactions : [];
  }

  @computed get hasAny() {
    const { result } = this.searchRequest;
    return result ? result.total > 0 : false;
  }

  @computed get totalAvailable() {
    const { result } = this.searchRequest;
    return result ? result.total : 0;
  }
}
