// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';

export default class AppStore extends Store {

  @observable isInitialized = false;

  constructor(...args) {
    super(...args);
    this.actions.goToRoute.listen(this._updateRouteLocation);
  }

  _updateRouteLocation = ({ route }) => {
    this.stores.routing.router.transitionTo(route);
  };

  @action initialize() {
    this.isInitialized = true;
  }
}
