// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import { ROUTES } from '../Routes';

export default class SidebarStore extends Store {

  CATEGORIES = {
    WALLETS: ROUTES.WALLETS.ROOT,
    ADA_REDEMPTION: ROUTES.ADA_REDEMPTION,
    SETTINGS: ROUTES.SETTINGS.ROOT,
  };

  INITIAL_HIDE_SUB_MENU_DELAY = 4000;
  ACTION_HIDE_SUB_MENU_DELAY = 1000;

  @observable activeSidebarCategory: string = this.CATEGORIES.WALLETS;
  @observable isShowingSubMenus: bool = true;

  _hideSubMenuTimeout = null;

  setup() {
    const actions = this.actions.sidebar;
    actions.toggleSubMenus.listen(this._toggleSubMenus);
    actions.activateSidebarCategory.listen(this._onActivateSidebarCategory);
    actions.walletSelected.listen(this._onWalletSelected);
    this.actions.networkStatus.isSyncedAndReady.listen(() => {
      this._hideSubMenusAfterDelay(this.INITIAL_HIDE_SUB_MENU_DELAY);
    });
    this.registerReactions([
      this._syncSidebarRouteWithRouter,
    ]);
  }

  @computed get wallets(): Array<SidebarWalletType> {
    const { wallets, networkStatus } = this.stores;
    return wallets.all.map(w => ({
      id: w.id,
      title: w.name,
      info: `${w.amount.toFormat()} ${w.currency}`,
      isConnected: networkStatus.isConnected,
    }));
  }

  @action _toggleSubMenus = () => {
    this.isShowingSubMenus = !this.isShowingSubMenus;
    this._clearExistingHideSubMenuTimeout();
  };

  @action _hideSubMenus = () => {
    this.isShowingSubMenus = false;
    this._clearExistingHideSubMenuTimeout();
  };

  @action _onActivateSidebarCategory = (
      { category, showSubMenus }: { category: string, showSubMenus: boolean }
    ) => {
    this._clearExistingHideSubMenuTimeout();
    if (category !== this.activeSidebarCategory) {
      this.activeSidebarCategory = category;
      if (showSubMenus != null) this.isShowingSubMenus = showSubMenus;
      this.actions.router.goToRoute({ route: category });
    } else if (showSubMenus == null || this.isShowingSubMenus !== showSubMenus) {
      // If no explicit preferred state is given -> toggle sub menus
      this._toggleSubMenus();
    } else {
      this.isShowingSubMenus = showSubMenus;
    }
  };

  @action _hideSubMenusAfterDelay = (delay: number) => {
    this._clearExistingHideSubMenuTimeout();
    this._hideSubMenuTimeout = setTimeout(this._hideSubMenus, delay);
  };

  @action _onWalletSelected = ({ walletId }: { walletId: string }) => {
    this.stores.wallets.goToWalletRoute(walletId);
    this._hideSubMenusAfterDelay(this.ACTION_HIDE_SUB_MENU_DELAY);
  };

  @action _setActivateSidebarCategory = (category: string) => {
    this.activeSidebarCategory = category;
  };

  _syncSidebarRouteWithRouter = () => {
    const route = this.stores.app.currentRoute;
    Object.keys(this.CATEGORIES).forEach((key) => {
      const category = this.CATEGORIES[key];
      // If the current route starts with the root of the category
      if (route.indexOf(category) === 0) this._setActivateSidebarCategory(category);
    });
  };

  _clearExistingHideSubMenuTimeout() {
    if (this._hideSubMenuTimeout) clearTimeout(this._hideSubMenuTimeout);
  }

}

export type SidebarWalletType = {
  id: string,
  title: string,
  info: string,
  isConnected: bool,
};
