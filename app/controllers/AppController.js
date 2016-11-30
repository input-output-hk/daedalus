// @flow
import { action } from 'mobx';
import type { appState } from '../state/index';
import AccountController from './AccountController';
import WalletsController from './WalletsController';
import SidebarController from './SidebarController';

export default class AppController {

  state: appState;
  router: { transitionTo: () => void };
  intl: { formatMessage: () => string };
  account: AccountController;
  wallets: WalletsController;
  sidebar: SidebarController;
  initializedCallback = () => {};

  constructor(state: appState) {
    this.state = state;
    this.account = new AccountController(this, state);
    this.wallets = new WalletsController(this, state);
    this.sidebar = new SidebarController(this, state);
    this.load();
  }

  load() {
    this.account.loadAccount();
    this.wallets.loadWallets();
  }

  onInitialized(callback: () => null) {
    this.initializedCallback = callback;
    if (this.state.isInitialized) {
      callback();
    }
  }

  @action initialize(router: Object, location: Object, intl: Object) {
    this.router = router;
    this.intl = intl;
    this.state.router = { location };
    this.state.isInitialized = true;
    this.initializedCallback();
  }

  setRouter(router: Object) {
    this.router = router;
  }

  setTranslationService(intl: Object) {
    this.intl = intl;
  }

  updateLocation(location: Object) {
    this.state.router.location = location;
  }

  navigateTo(route: string) {
    this.router.transitionTo(route);
  }

  translate(descriptor: Object, values: Object) {
    return this.intl.formatMessage(descriptor, values);
  }

  @action reset() {
    this.state.reset();
    this.load();
    this.state.isInitialized = true;
    this.initializedCallback();
  }

}
