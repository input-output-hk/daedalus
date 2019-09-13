// @flow
import { action, computed, observable } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import { sidebarConfig } from '../config/sidebarConfig';
import { WalletSyncStateStatuses } from '../domains/Wallet';
import { formattedWalletAmount } from '../utils/formatters';
import type { SidebarWalletType } from '../types/sidebarTypes';

export default class SidebarStore extends Store {
  @observable CATEGORIES: Array<any> = sidebarConfig.CATEGORIES;
  @observable activeSidebarCategory: string = this.CATEGORIES[0].route;
  @observable isShowingSubMenus: boolean = true;

  setup() {
    const { sidebar: sidebarActions } = this.actions;

    sidebarActions.showSubMenus.listen(this._showSubMenus);
    sidebarActions.toggleSubMenus.listen(this._toggleSubMenus);
    sidebarActions.activateSidebarCategory.listen(
      this._onActivateSidebarCategory
    );
    sidebarActions.walletSelected.listen(this._onWalletSelected);

    this.registerReactions([this._syncSidebarRouteWithRouter]);
    this._configureCategories();
  }

  @computed get wallets(): Array<SidebarWalletType> {
    const { networkStatus, wallets } = this.stores;
    return wallets.all.map(wallet => ({
      id: wallet.id,
      title: wallet.name,
      info: formattedWalletAmount(wallet.amount),
      isConnected: networkStatus.isConnected,
      isRestoreActive:
        get(wallet, ['syncState', 'status'], '') ===
        WalletSyncStateStatuses.RESTORING,
      restoreProgress: get(wallet, ['syncState', 'progress', 'quantity'], 0),
      isLegacy: wallet.isLegacy,
    }));
  }

  @action _configureCategories = () => {
    if (this.stores.networkStatus.environment.isDev) {
      this.CATEGORIES = sidebarConfig.CATEGORIES_WITH_STAKING;
    }
  };

  @action _onActivateSidebarCategory = (params: {
    category: string,
    showSubMenu?: boolean,
  }) => {
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
    this.stores.wallets.goToWalletRoute(walletId);
  };

  @action _setActivateSidebarCategory = (category: string) => {
    this.activeSidebarCategory = category;
  };

  @action _resetActivateSidebarCategory = () => {
    this.activeSidebarCategory = '';
  };

  @action _showSubMenus = () => {
    this.isShowingSubMenus = true;
  };

  @action _hideSubMenus = () => {
    this.isShowingSubMenus = false;
  };

  @action _toggleSubMenus = () => {
    this.isShowingSubMenus = !this.isShowingSubMenus;
  };

  _syncSidebarRouteWithRouter = () => {
    const route = this.stores.app.currentRoute;
    this.CATEGORIES.forEach(category => {
      // If the current route starts with the root of the category
      if (route.indexOf(category.route) === 0)
        this._setActivateSidebarCategory(category.route);
    });
  };
}
