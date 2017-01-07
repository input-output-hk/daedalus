// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import { matchRoute } from '../lib/routing-helpers';
import CachedRequest from './lib/CachedRequest';

export default class SidebarStore extends Store {

  @observable route: string = '/wallets';
  @observable hidden: bool = false;
  @observable isMaximized: bool = false;
  @observable isCreateWalletDialogOpen = false;

  constructor(...args) {
    super(...args);
  }

  @computed get wallets() {
    return this.stores.wallets.all.map(w => ({
      id: w.id,
      title: w.name,
      info: `${w.amount} ${w.currency}`
    }));
  }

}
