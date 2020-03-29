// @flow
import { action, computed, observable } from 'mobx';
import Store from './lib/Store';
import { sidebarConfig } from '../config/sidebarConfig';
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

  // We need to use computed.struct for computed objects (so they are structurally compared
  // for equality instead of idendity (which would always invalidate)
  // https://alexhisen.gitbooks.io/mobx-recipes/content/use-computedstruct-for-computed-objects.html
  @computed.struct get wallets(): Array<SidebarWalletType> {
    const { networkStatus, wallets } = this.stores;
    return wallets.all.map(wallet => {
      const {
        recoveryPhraseVerificationStatus,
      } = wallets.getWalletRecoveryPhraseVerification(wallet.id);
      return {
        id: wallet.id,
        title: wallet.name,
        info: formattedWalletAmount(wallet.amount, true, false),
        isConnected: networkStatus.isConnected,
        isRestoreActive: wallet.isRestoring,
        restoreProgress: wallet.restorationProgress,
        isNotResponding: wallet.isNotResponding,
        isLegacy: wallet.isLegacy,
        recoveryPhraseVerificationStatus,
      };
    });
  }

  @action _configureCategories = () => {
    const { isIncentivizedTestnet, isFlight } = global;
    if (isIncentivizedTestnet) {
      this.CATEGORIES = sidebarConfig.CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN;
    } else if (isFlight) {
      this.CATEGORIES = sidebarConfig.CATEGORIES;
    } else {
      this.CATEGORIES = sidebarConfig.CATEGORIES_WITHOUT_NETWORK_INFO;
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
