// @flow
import { action } from 'mobx';
import Route from 'route-parser';
import type { appState } from '../state/index';
import type { Api } from '../api';
import UserController from './UserController';
import WalletsController from './WalletsController';
import SidebarController from './SidebarController';
import SettingsController from './SettingsController';
import { storesType } from '../stores';

export default class AppController {

  state: appState;
  api: Api;
  stores: storesType;
  router: { transitionTo: () => void };
  intl: { formatMessage: () => string };
  user: UserController;
  wallets: WalletsController;
  sidebar: SidebarController;
  settings: SettingsController;
  initializedCallback = () => {};

  constructor(state: appState, api: Api, stores: storesType) {
    this.state = state;
    this.api = api;
    this.stores = stores;
    this.user = new UserController(this, state, api);
    this.wallets = new WalletsController(this, state, api);
    this.sidebar = new SidebarController(this, state, api);
    this.settings = new SettingsController(this, state, api);
  }

  onInitialized(callback: () => null) {
    this.initializedCallback = callback;
    if (this.stores.app.isInitialized) {
      callback();
    }
  }

  @action initialize(router: Object, location: Object, intl: Object) {
    this.router = router;
    this.intl = intl;
    this.state.router = { location };
    this.stores.app.initialize();
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
    this.handleRouteChange(location.pathname);
  }

  navigateTo(route: string) {
    this.router.transitionTo(route);
  }

  translate(descriptor: Object, values: Object) {
    return this.intl.formatMessage(descriptor, values);
  }

  @action reset() {
    this.state.reset();
    this.initialize(this.router, this.state.router, this.intl);
    this.initializedCallback();
  }

  // TODO: Refactor this into dedicated handlers later on! (event-bus arch)
  handleRouteChange(path:string) {
    const walletRoute = new Route('/wallet/:id/:screen').match(path);

    if (walletRoute) {
      this.wallets.setActiveWallet(walletRoute.id);
    }
  }
}
