// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import { matchRoute } from '../lib/routing-helpers';
import CachedRequest from './lib/CachedRequest';

const WALLETS_BASE_ROUTE = '/wallets';

export default class WalletsStore extends Store {

  @observable walletsRequest = new CachedRequest(this.api, 'getWallets');

  constructor(...args) {
    super(...args);
  }

  @computed get all() {
    return this.walletsRequest.execute(this.stores.user.active.id).result || [];
  }

  @computed get active() {
    const currentRoute = this.stores.routing.location.pathname;
    const match = matchRoute(`${WALLETS_BASE_ROUTE}/:id(*page)`, currentRoute);
    if (match) return this.all.find(w => w.id === match.id);
    return this.all[0];
  }

}
