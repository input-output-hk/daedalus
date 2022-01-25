// @flow
import { action, computed, observable } from 'mobx';
import { get } from 'lodash';
import { sidebarConfig } from '../config/sidebarConfig';
import type { SidebarCategoryInfo } from '../config/sidebarConfig';
import type {
  SidebarWalletType,
  WalletSortConfig,
  WalletSortByOptions,
} from '../types/sidebarTypes';
import { WalletSortBy, WalletSortOrder } from '../types/sidebarTypes';
import { changeWalletSorting, sortWallets } from '../utils/walletSorting';
import Store from './lib/Store';

export default class SidebarStore extends Store {
  @observable CATEGORIES: Array<any> = sidebarConfig.CATEGORIES_LIST;
  @observable activeSidebarCategory: string = this.CATEGORIES[0].route;
  @observable isShowingSubMenus: boolean = true;
  @observable walletSortConfig: WalletSortConfig = {
    sortBy: WalletSortBy.Date,
    sortOrder: WalletSortOrder.Asc,
  };
  @observable searchValue: string = '';

  setup() {
    const { sidebar: sidebarActions } = this.actions;
    sidebarActions.showSubMenus.listen(this._showSubMenus);
    sidebarActions.toggleSubMenus.listen(this._toggleSubMenus);
    sidebarActions.activateSidebarCategory.listen(
      this._onActivateSidebarCategory
    );
    sidebarActions.walletSelected.listen(this._onWalletSelected);
    this.registerReactions([
      this._syncSidebarRouteWithRouter,
      this._syncSidebarItemsWithShelleyActivation,
    ]);
    this._configureCategories();
  }

  // We need to use computed.struct for computed objects (so they are structurally compared
  // for equality instead of idendity (which would always invalidate)
  // https://alexhisen.gitbooks.io/mobx-recipes/content/use-computedstruct-for-computed-objects.html
  @computed.struct get wallets(): Array<SidebarWalletType> {
    const {
      networkStatus,
      wallets,
      walletSettings,
      hardwareWallets,
    } = this.stores;
    const { hardwareWalletsConnectionData } = hardwareWallets;
    const shelleyWallets = sortWallets({
      wallets: wallets.all.filter((w) => !w.isLegacy),
      sortOrder: this.walletSortConfig.sortOrder,
      sortBy: this.walletSortConfig.sortBy,
    });
    const byronWallets = sortWallets({
      wallets: wallets.all.filter((w) => w.isLegacy),
      sortOrder: this.walletSortConfig.sortOrder,
      sortBy: this.walletSortConfig.sortBy,
    });
    const sortedWallets = [...shelleyWallets, ...byronWallets];
    return sortedWallets.map((wallet) => {
      const isHardwareWalletDisconnected = get(
        hardwareWalletsConnectionData,
        [wallet.id, 'disconnected'],
        true
      );
      const {
        hasNotification,
      } = walletSettings.getWalletsRecoveryPhraseVerificationData(wallet.id);
      return {
        id: wallet.id,
        title: wallet.name,
        amount: wallet.amount,
        isConnected: networkStatus.isConnected,
        isRestoreActive: wallet.isRestoring,
        restoreProgress: wallet.restorationProgress,
        isNotResponding: wallet.isNotResponding,
        isLegacy: wallet.isLegacy,
        isHardwareWallet: wallet.isHardwareWallet,
        isHardwareWalletDisconnected,
        hasNotification,
      };
    });
  }

  @action onChangeWalletSortType = (sortBy: WalletSortByOptions) => {
    this.walletSortConfig = changeWalletSorting({
      sortBy,
      sortOrder: this.walletSortConfig.sortOrder,
      currentSortBy: this.walletSortConfig.sortBy,
    });
  };

  @action onSearchValueUpdated = (searchValue: string) => {
    this.searchValue = searchValue;
  };

  @action _configureCategories = () => {
    const {
      isFlight,
      environment: { isDev, isMainnet },
    } = global;

    const {
      CATEGORIES_BY_NAME: categories,
      CATEGORIES_LIST: list,
    } = sidebarConfig;

    const categoryValidation: {
      [key: string]: boolean | Function,
    } = {
      [categories.WALLETS.name]: true,
      [categories.PAPER_WALLET_CREATE_CERTIFICATE.name]: false,
      [categories.STAKING_DELEGATION_COUNTDOWN.name]: false,
      [categories.STAKING.name]: true,
      [categories.SETTINGS.name]: true,
      [categories.VOTING.name]: isMainnet || isDev,
      [categories.NETWORK_INFO.name]: isFlight,
    };

    const categoriesFilteredList: Array<SidebarCategoryInfo> = list.filter(
      ({ name }: SidebarCategoryInfo): boolean => {
        let validator = categoryValidation[name];
        if (typeof validator === 'function') {
          validator = validator();
        }
        return validator;
      }
    );

    this.CATEGORIES = categoriesFilteredList;
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
    this.CATEGORIES.forEach((category) => {
      // If the current route starts with the root of the category
      if (route.indexOf(category.route) === 0)
        this._setActivateSidebarCategory(category.route);
    });
  };

  _syncSidebarItemsWithShelleyActivation = () => {
    const { isShelleyActivated, isShelleyPending } = this.stores.networkStatus;
    if (isShelleyActivated || isShelleyPending) {
      this._configureCategories();
    }
  };
}
