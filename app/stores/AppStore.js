// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';

export default class AppStore extends Store {

  @observable isInitialized = false;
  @observable currentLocale = 'en-US';

  _initializedCallback = null;

  constructor(stores, api, actions, initializedCallback) {
    super(stores, api, actions);
    this._initializedCallback = initializedCallback;
    this.actions.goToRoute.listen(this._updateRouteLocation);
  }

  _updateRouteLocation = ({ route }) => {
    this.stores.routing.router.transitionTo(route);
  };

  @action initialize(router: Object, intl: Object) {
    this.stores.routing.router = router;
    this.intl = intl;
    this.isInitialized = true;
    if (this._initializedCallback) this._initializedCallback();
  }

  @action updateLocation(location) {
    this.stores.routing.location = location;
  }

  translate(...args) {
    return this.intl.formatMessage(...args);
  }
}
