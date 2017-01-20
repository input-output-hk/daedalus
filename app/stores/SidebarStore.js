// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';

export default class SidebarStore extends Store {

  @observable route: string = '/wallets';
  @observable hidden: bool = false;
  @observable isMaximized: bool = false;
  @observable isCreateWalletDialogOpen = false;

  constructor(...args) {
    super(...args);
    this.actions.toggleSidebar.listen(this._toggleSidebar);
    this.actions.changeSidebarRoute.listen(this._changeSidebarRoute);
    this.actions.toggleCreateWalletDialog.listen(this._toggleCreateWalletDialog);
  }

  @computed get wallets() {
    const { wallets, networkStatus } = this.stores;
    return wallets.all.map(w => ({
      id: w.id,
      title: w.name,
      info: `${w.amount} ${w.currency}`,
      networkStatus: networkStatus.cardanoStatus,
    }));
  }

  @action _toggleSidebar = () => {
    this.hidden = !this.hidden;
  };

  @action _toggleCreateWalletDialog = () => {
    this.isCreateWalletDialogOpen = !this.isCreateWalletDialogOpen;
  };

  @action _changeSidebarRoute = ({ route }) => {
    if (this.route === route) {
      // Toggle menu if it's the current route
      this.isMaximized = !this.isMaximized;
    } else {
      this.route = route;
      this.isMaximized = false;
      if (route === '/settings' || route === '/staking') {
        this.stores.router.push(route);
      }
    }
  }

}
