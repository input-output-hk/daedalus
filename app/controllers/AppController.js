// @flow
import { action } from 'mobx';
import type { Api } from '../api';
import { storesType } from '../stores';

export default class AppController {

  api: Api;
  stores: storesType;
  router: { transitionTo: () => void };
  intl: { formatMessage: () => string };
  initializedCallback = () => {};

  constructor(api: Api, stores: storesType) {
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
    Object.assign(this.stores.routing, { router, location });
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
    this.stores.routing.location = location;
  }

  navigateTo(route: string) {
    this.router.transitionTo(route);
  }

  translate(descriptor: Object, values: Object) {
    return this.intl.formatMessage(descriptor, values);
  }

  @action reset() {
    this.initialize(this.router, this.stores.routing.location, this.intl);
    this.initializedCallback();
  }
}
