// @flow
import { action } from 'mobx';
import type { appState } from '../state/index';
import type { Api } from '../api';
import { storesType } from '../stores';

export default class AppController {

  state: appState;
  api: Api;
  stores: storesType;
  router: { transitionTo: () => void };
  intl: { formatMessage: () => string };
  initializedCallback = () => {};

  constructor(state: appState, api: Api, stores: storesType) {
    this.state = state;
    this.api = api;
    this.stores = stores;
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
}
