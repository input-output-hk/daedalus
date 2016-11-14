// @flow
import { observable, computed } from 'mobx';
import AppStore from '../AppStore';

export default class SidebarState {

  store: AppStore;
  @observable route: string;
  @observable hidden: bool;
  @observable showMenu: bool;

  constructor(store: AppStore) {
    Object.assign(this, {
      route: '/wallets',
      hidden: false,
      showMenu: true,
    });
    this.store = store;
  }

  @computed get wallets(): Array<Object> {
    return this.store.account.wallets.map(wallet => ({
      id: wallet.address,
      title: wallet.name,
      info: `${wallet.amount} ${wallet.currency}`
    }));
  }
}
