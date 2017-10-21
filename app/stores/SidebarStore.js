// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import resolver from '../lib/resolver';
import { ROUTES } from '../routes-config';
import { matchRoute } from './../lib/routing-helpers';
import environment from '../environment';

const sidebarConfig = resolver('config/sidebarConfig');
const { formattedWalletAmount } = resolver('helpers/formatters');

export default class SidebarStore extends Store {

  CATEGORIES = sidebarConfig.CATEGORIES;

  @observable activeSidebarCategory: string = this.CATEGORIES.WALLETS;
  @observable isShowingSubMenus: boolean = true;

  setup() {
    const actions = this.actions.sidebar;
    actions.toggleSubMenus.listen(this._toggleSubMenus);
    actions.activateSidebarCategory.listen(this._onActivateSidebarCategory);
    actions.walletSelected.listen(this._onWalletSelected);
    this.registerReactions([
      this._syncSidebarRouteWithRouter,
      this._showSubMenusOnWalletsPageLoad,
    ]);
  }

  @computed get wallets(): Array<SidebarWalletType> {
    const { networkStatus } = this.stores;
    const { wallets } = this.stores[environment.API];
    return wallets.all.map(w => ({
      id: w.id,
      title: w.name,
      info: formattedWalletAmount(w.amount),
      isConnected: networkStatus.isConnected,
    }));
  }

  @action _toggleSubMenus = () => {
    this.isShowingSubMenus = !this.isShowingSubMenus;
  };

  @action _onActivateSidebarCategory = (params: { category: string, showSubMenu?: boolean }) => {
    const { category, showSubMenu } = params;
    if (category !== this.activeSidebarCategory) {
      this.activeSidebarCategory = category;
      if (showSubMenu != null) this.isShowingSubMenus = showSubMenu;
      this.actions.router.goToRoute.trigger({ route: category });
    } else if (showSubMenu == null || this.isShowingSubMenus !== showSubMenu) {
      // If no explicit preferred state is given -> toggle sub menus
      this._toggleSubMenus();
    } else {
      this.isShowingSubMenus = showSubMenu;
    }
  };

  @action _onWalletSelected = ({ walletId }: { walletId: string }) => {
    this.stores[environment.API].wallets.goToWalletRoute(walletId);
  };

  @action _setActivateSidebarCategory = (category: string) => {
    this.activeSidebarCategory = category;
  };

  @action _showSubMenus = () => {
    this.isShowingSubMenus = true;
  };

  _syncSidebarRouteWithRouter = () => {
    const route = this.stores.app.currentRoute;
    Object.keys(this.CATEGORIES).forEach((key) => {
      const category = this.CATEGORIES[key];
      // If the current route starts with the root of the category
      if (route.indexOf(category) === 0) this._setActivateSidebarCategory(category);
    });
  };

  _showSubMenusOnWalletsPageLoad = () => {
    const currentRoute = this.stores.app.currentRoute;
    if (matchRoute(ROUTES.WALLETS.ROOT, currentRoute)) {
      this._showSubMenus();
    }
  }

}

export type SidebarWalletType = {
  id: string,
  title: string,
  info: string,
  isConnected: bool,
};
