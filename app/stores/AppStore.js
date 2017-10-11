// @flow
import { observable, computed } from 'mobx';
import Store from './lib/Store';
import LocalizableError from '../i18n/LocalizableError';
import { buildRoute } from '../lib/routing-helpers';
import { ROUTES } from "../routes-config";

export default class AppStore extends Store {

  @observable error: ?LocalizableError = null;

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.registerReactions([
      this._redirectToLoadingScreenWhenDisconnected,
    ]);
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  _redirectToLoadingScreenWhenDisconnected = () => {
    if (!this.stores.networkStatus.isConnected) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
    }
  };

  _updateRouteLocation = (options: { route: string, params: ?Object }) => {
    const routePath = buildRoute(options.route, options.params);
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== routePath) this.stores.router.push(routePath);
  };

}
