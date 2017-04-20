// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import { ROUTES } from '../Routes';
import { DECIMAL_PLACES_IN_ADA } from '../config/numbersConfig';

export default class SidebarStore extends Store {

  CATEGORIES = {
    WALLETS: ROUTES.WALLETS.ROOT,
    ADA_REDEMPTION: ROUTES.ADA_REDEMPTION,
    SETTINGS: ROUTES.SETTINGS.ROOT,
  };

  DEFAULT_WINDOW_WIDTH = 1150;

  @observable activeSidebarCategory: string = this.CATEGORIES.WALLETS;
  @observable isShowingSubMenus: bool = false;

  setup() {
    const actions = this.actions.sidebar;
    actions.toggleSubMenus.listen(this._toggleSubMenus);
    actions.activateSidebarCategory.listen(this._onActivateSidebarCategory);
    actions.walletSelected.listen(this._onWalletSelected);
    this.registerReactions([
      this._syncSidebarRouteWithRouter,
      this._setInitialSubMenusState,
    ]);
  }

  @computed get wallets(): Array<SidebarWalletType> {
    const { wallets, networkStatus } = this.stores;
    return wallets.all.map(w => ({
      id: w.id,
      title: w.name,
      info: `${w.amount.toFormat(DECIMAL_PLACES_IN_ADA)} ${w.currency}`,
      isConnected: networkStatus.isConnected,
    }));
  }

  @action _toggleSubMenus = () => {
    this.isShowingSubMenus = !this.isShowingSubMenus;
    window.removeEventListener('resize', this._handleWindowResize);
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

  @action _handleWindowResize = () => {
    if (document.documentElement.clientWidth < this.DEFAULT_WINDOW_WIDTH) {
      this.isShowingSubMenus = false;
    } else {
      this.isShowingSubMenus = true;
    }
  };

  @action _onWalletSelected = ({ walletId }: { walletId: string }) => {
    this.stores.wallets.goToWalletRoute(walletId);
  };

  @action _setActivateSidebarCategory = (category: string) => {
    this.activeSidebarCategory = category;
  };

  _setInitialSubMenusState = () => {
    if (document.documentElement.clientWidth >= this.DEFAULT_WINDOW_WIDTH) {
      this.isShowingSubMenus = true;
    }
    window.addEventListener('resize', this._handleWindowResize);
  };

  _syncSidebarRouteWithRouter = () => {
    const route = this.stores.app.currentRoute;
    Object.keys(this.CATEGORIES).forEach((key) => {
      const category = this.CATEGORIES[key];
      // If the current route starts with the root of the category
      if (route.indexOf(category) === 0) this._setActivateSidebarCategory(category);
    });
  };

}

export type SidebarWalletType = {
  id: string,
  title: string,
  info: string,
  isConnected: bool,
};
