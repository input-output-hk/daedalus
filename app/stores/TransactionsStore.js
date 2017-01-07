// @flow
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';

export default class TransactionsStore extends Store {

  static INITIAL_SEARCH_LIMIT = 10;
  static SEARCH_LIMIT_INCREASE = 10;

  @observable searchTerm: string = '';
  @observable searchLimit: number = TransactionsStore.INITIAL_SEARCH_LIMIT;

  @observable searchRequest = new CachedRequest(this.api, 'getTransactions');

  constructor(...args) {
    super(...args);
    this.actions.filterTransactions.listen(this._updateSearchTerm);
    this.actions.loadMoreTransactions.listen(this._increaseSearchLimit);
  }

  @action _updateSearchTerm = ({ searchTerm }) => this.searchTerm = searchTerm;
  @action _increaseSearchLimit = () => this.searchLimit += TransactionsStore.SEARCH_LIMIT_INCREASE;

  @computed get filtered() {
    const { result } = this.searchRequest.execute({
      walletId: this.stores.wallets.active.id,
      searchTerm: this.searchTerm,
      limit: this.searchLimit
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
