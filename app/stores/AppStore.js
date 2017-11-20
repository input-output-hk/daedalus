// @flow
import { observable, computed } from 'mobx';
import Store from './lib/Store';
import LocalizableError from '../i18n/LocalizableError';
import { buildRoute } from '../utils/routing';

export default class AppStore extends Store {

  @observable error: ?LocalizableError = null;

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  _updateRouteLocation = (options: { route: string, params: ?Object }) => {
    const routePath = buildRoute(options.route, options.params);
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== routePath) this.stores.router.push(routePath);
  };
}
