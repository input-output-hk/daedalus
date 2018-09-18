// @flow
import { action, computed, observable } from 'mobx';
import { get } from 'lodash';
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import environment from '../../../common/environment';
import { sidebarConfig } from '../config/sidebarConfig';
import { WalletSyncStateTags } from '../domains/Wallet';
import { GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL } from '../../../common/ipc-api/go-to-ada-redemption-screen';
import { formattedWalletAmount } from '../utils/formatters';
import type { SidebarWalletType } from '../types/sidebarTypes';

export default class SidebarStore extends Store {

  CATEGORIES = sidebarConfig.CATEGORIES;

  @observable activeSidebarCategory: string = this.CATEGORIES[0].route;
  @observable isShowingSubMenus: boolean = true;

  setup() {
    const actions = this.actions.sidebar;
    actions.showSubMenus.listen(this._showSubMenus);
    actions.toggleSubMenus.listen(this._toggleSubMenus);
    actions.activateSidebarCategory.listen(this._onActivateSidebarCategory);
    actions.walletSelected.listen(this._onWalletSelected);
    ipcRenderer.on(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL, this._resetActivateSidebarCategory);
    this.registerReactions([
      this._syncSidebarRouteWithRouter,
    ]);
  }

  teardown() {
    ipcRenderer.removeListener(
      GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL,
      this._resetActivateSidebarCategory
    );
  }

  @computed get wallets(): Array<SidebarWalletType> {
    const { networkStatus } = this.stores;
    const { wallets } = this.stores[environment.API];
    return wallets.all.map(w => ({
      id: w.id,
      title: w.name,
      info: formattedWalletAmount(w.amount),
      isConnected: networkStatus.isConnected,
      isRestoreActive: get(w, 'syncState.tag') === WalletSyncStateTags.RESTORING,
      restoreProgress: get(w, 'syncState.data.percentage.quantity', 0),
    }));
  }

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
    this.CATEGORIES.forEach((category) => {
      // If the current route starts with the root of the category
      if (route.indexOf(category.route) === 0) this._setActivateSidebarCategory(category.route);
    });
  };

}
