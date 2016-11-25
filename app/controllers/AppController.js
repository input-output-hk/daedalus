// @flow
import { action } from 'mobx';
import type { appState } from '../state/index';
import AccountController from './AccountController';
import WalletsController from './WalletsController';
import SidebarController from './SidebarController';

export default class AppController {

  state: appState;
  account: AccountController;
  wallets: WalletsController;
  sidebar: SidebarController;
  initializedCallback = () => {};

  constructor(state: appState) {
    this.state = state;
    this.account = new AccountController(state);
    this.wallets = new WalletsController(state);
    this.sidebar = new SidebarController(state);

    this.account.loadAccount();
    this.wallets.loadWallets();
  }

  onInitialized(callback: () => {}) {
    this.initializedCallback = callback;
  }

  @action initialize(router: Object, intl: Object) {
    this.state.router = router;
    this.state.i18n.intl = intl;
    this.initializedCallback();
  }

  navigateTo(route: string) {
    if (!this.state.router) throw new Error('Cannot to navigate because router is not yet set.');
    this.state.router.transitionTo(route);
  }

}
