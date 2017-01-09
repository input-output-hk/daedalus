// @flow
import { observable } from 'mobx';
import Store from './lib/Store';

export default class AppStore extends Store {

  @observable currentLocale = 'en-US';

  constructor(stores, api, actions) {
    super(stores, api, actions);
    this.actions.goToRoute.listen(this._updateRouteLocation);
  }

  _updateRouteLocation = ({ route }) => {
    this.stores.router.push(route);
  };

}
