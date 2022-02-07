import { action, computed, observable } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import { sidebarConfig } from '../config/sidebarConfig';
import type { SidebarCategoryInfo } from '../config/sidebarConfig';
import type { SidebarWalletType } from '../types/sidebarTypes';

export default class SidebarStore extends Store {
  @observable
  CATEGORIES: Array<any> = sidebarConfig.CATEGORIES_LIST;
  @observable
  activeSidebarCategory: string = this.CATEGORIES[0].route;
  @observable
  isShowingSubMenus = true;

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
  @computed.struct
  get wallets(): Array<SidebarWalletType> {
    const {
      networkStatus,
      wallets,
      walletSettings,
      hardwareWallets,
    } = this.stores;
    const { hardwareWalletsConnectionData } = hardwareWallets;
    return wallets.all.map((wallet) => {
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

  @action
  _configureCategories = () => {
    const {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
      isFlight,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'environment' does not exist on type 'typ... Remove this comment to see the full error message
      environment: { isDev, isMainnet },
    } = global;
    const {
      CATEGORIES_BY_NAME: categories,
      CATEGORIES_LIST: list,
    } = sidebarConfig;
    const categoryValidation: Record<
      string,
      boolean | ((...args: Array<any>) => any)
    > = {
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

        // @ts-ignore ts-migrate(2322) FIXME: Type 'boolean | ((...args: any[]) => any)' is not ... Remove this comment to see the full error message
        return validator;
      }
    );
    this.CATEGORIES = categoriesFilteredList;
  };
  @action
  _onActivateSidebarCategory = (params: {
    category: string;
    showSubMenu?: boolean;
  }) => {
    const { category, showSubMenu } = params;

    if (category !== this.activeSidebarCategory) {
      this.activeSidebarCategory = category;
      if (showSubMenu != null) this.isShowingSubMenus = showSubMenu;
      this.actions.router.goToRoute.trigger({
        route: category,
      });
    } else if (showSubMenu == null || this.isShowingSubMenus !== showSubMenu) {
      // If no explicit preferred state is given -> toggle sub menus
      this._toggleSubMenus();
    } else {
      this.isShowingSubMenus = showSubMenu;
    }
  };
  @action
  _onWalletSelected = ({ walletId }: { walletId: string }) => {
    this.stores.wallets.goToWalletRoute(walletId);
  };
  @action
  _setActivateSidebarCategory = (category: string) => {
    this.activeSidebarCategory = category;
  };
  @action
  _resetActivateSidebarCategory = () => {
    this.activeSidebarCategory = '';
  };
  @action
  _showSubMenus = () => {
    this.isShowingSubMenus = true;
  };
  @action
  _hideSubMenus = () => {
    this.isShowingSubMenus = false;
  };
  @action
  _toggleSubMenus = () => {
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
