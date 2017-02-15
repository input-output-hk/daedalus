// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';

export default class SidebarStore extends Store {

  CATEGORIES = {
    WALLETS: '/wallets',
    ADA_REDEMPTION: '/ada-redemption',
  };

  @observable currentCategory: ?string = null;
  @observable hidden: bool = false;
  @observable isMaximized: bool = false;

  constructor(...args) {
    super(...args);
    this.actions.toggleSidebar.listen(this._toggleSidebar);
    this.actions.toggleMaximized.listen(this._toggleMaximized);
    this.actions.sidebarCategorySelected.listen(this._onSidebarCategorySelected);
    this.registerReactions([
      this._syncSidebarRouteWithRouter,
    ]);
  }

  @computed get wallets() {
    const { wallets, networkStatus } = this.stores;
    return wallets.all.map(w => ({
      id: w.id,
      title: w.name,
      info: `${w.amount} ${w.currency}`,
      isConnected: networkStatus.isConnected,
    }));
  }

  @action _toggleSidebar = () => {
    this.hidden = !this.hidden;
  };

  @action _toggleMaximized = () => {
    this.isMaximized = !this.isMaximized;
  };

  @action _onSidebarCategorySelected = ({ category }) => {
    if (category === this.currentCategory) {
      this._toggleMaximized();
    } else {
      this.stores.router.push(category);
    }
    this.currentCategory = category;
  };

  _syncSidebarRouteWithRouter = () => {
    const route = this.stores.router.location.pathname;
    Object.keys(this.CATEGORIES).forEach((key) => {
      const category = this.CATEGORIES[key];
      if (route.indexOf(category) !== -1) this.currentCategory = category;
    })
  }

}
