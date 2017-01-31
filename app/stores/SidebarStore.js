// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';

export default class SidebarStore extends Store {

  @observable route: string = '/wallets';
  @observable hidden: bool = false;
  @observable isMaximized: bool = false;

  constructor(...args) {
    super(...args);
    this.actions.toggleSidebar.listen(this._toggleSidebar);
    this.actions.changeSidebarRoute.listen(this._changeSidebarRoute);
  }

  @computed get wallets() {
    const { wallets, networkStatus } = this.stores;
    return wallets.all.map(w => ({
      id: w.id,
      title: w.name,
      info: `${w.amount} ${w.currency}`,
      isConnected: networkStatus.isCardanoConnected,
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
      if (route === '/settings' || route === '/ada-redemption') {
        this.stores.router.push(route);
      }
    }
  };

}
