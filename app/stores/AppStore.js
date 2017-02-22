// @flow
import { observable, computed } from 'mobx';
import Store from './lib/Store';

export default class AppStore extends Store {

  @observable currentLocale = 'en-US';

  setup() {
    this.actions.goToRoute.listen(this._updateRouteLocation);
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  _updateRouteLocation = ({ route }: { route: string }) => {
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== route) this.stores.router.push(route);
  };

}
