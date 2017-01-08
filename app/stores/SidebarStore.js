// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import { matchRoute } from '../lib/routing-helpers';
import CachedRequest from './lib/CachedRequest';

export default class SidebarStore extends Store {

  @observable route: string = '/wallets';
  @observable hidden: bool = false;
  @observable isMaximized: bool = false;
  @observable isCreateWalletDialogOpen = false;

  constructor(...args) {
    super(...args);
    this.actions.toggleSidebar.listen(this._toggleSidebar);
    this.actions.changeSidebarRoute.listen(this._changeSidebarRoute);
  }

  @computed get wallets() {
    return this.stores.wallets.all.map(w => ({
      id: w.id,
      title: w.name,
      info: `${w.amount} ${w.currency}`
    }));
  }

  @action _toggleSidebar = () => {
    this.hidden = !this.hidden;
  };

  @action _changeSidebarRoute = ({ route }) => {
    if (this.route === route) {
      // Toggle menu if it's the current route
      this.isMaximized = !this.isMaximized;
    } else {
      this.route = route;
      this.isMaximized = false;
      if (route === '/settings' || route === '/staking') {
        this.stores.routing.router.transitionTo(route);
      }
    }
  }

}
