// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';

export default class SidebarStore extends Store {

  @observable route: string = '/wallets';
  @observable hidden: bool = false;
  @observable isMaximized: bool = false;
  @observable isAddWalletDialogOpen = false;
  @observable isCreateWalletDialogOpen = false;
  @observable isWalletImportDialogOpen = false;

  constructor(...args) {
    super(...args);
    this.actions.toggleSidebar.listen(this._toggleSidebar);
    this.actions.changeSidebarRoute.listen(this._changeSidebarRoute);
    this.actions.toggleAddWallet.listen(this._toggleAddWallet);
    this.actions.toggleCreateWalletDialog.listen(this._toggleCreateWalletDialog);
    this.actions.toggleWalletImport.listen(this._toggleWalletImport);
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

  @action _toggleCreateWalletDialog = () => {
    if (!this.isCreateWalletDialogOpen) {
      this.isAddWalletDialogOpen = false;
      this.isCreateWalletDialogOpen = true;
    } else {
      this.isCreateWalletDialogOpen = false;
    }
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
  };

  @action _toggleAddWallet = () => {
    this.isAddWalletDialogOpen = !this.isAddWalletDialogOpen;
  };

  @action _toggleWalletImport = () => {
    if (!this.isWalletImportDialogOpen) {
      this.isAddWalletDialogOpen = false;
      this.isWalletImportDialogOpen = true;
    } else {
      this.isWalletImportDialogOpen = false;
    }
  };

}
