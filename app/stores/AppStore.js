// @flow
import { observable, action } from 'mobx';
import SidebarState from './state/SidebarState';
import WalletsState from './state/WalletsState';
import Account from '../domain/Account';

export default class AppStore {

  // Domain
  @observable account: Account;
  // Ui
  @observable sidebar: ?SidebarState;
  @observable wallets: ?WalletsState;
  @observable router: ?Object;
  @observable i18n: Object;

  constructor() {
    this.account = new Account();
    this.router = null;
    this.i18n = { locale: 'en-US' };
    this.sidebar = new SidebarState(this);
    this.wallets = new WalletsState(this);
  }

  @action setRouter(router: Object) {
    this.router = router;
  }
}
