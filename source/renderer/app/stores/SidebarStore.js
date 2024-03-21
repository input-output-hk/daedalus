'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const sidebarConfig_1 = require('../config/sidebarConfig');
const sidebarTypes_1 = require('../types/sidebarTypes');
const walletSorting_1 = require('../utils/walletSorting');
const Store_1 = __importDefault(require('./lib/Store'));
const analytics_1 = require('../analytics');
class SidebarStore extends Store_1.default {
  CATEGORIES = sidebarConfig_1.sidebarConfig.CATEGORIES_LIST;
  activeSidebarCategory = this.CATEGORIES[0].route;
  isShowingSubMenus = true;
  walletSortConfig = {
    sortBy: sidebarTypes_1.WalletSortBy.Date,
    sortOrder: sidebarTypes_1.WalletSortOrder.Asc,
  };
  searchValue = '';
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
  get wallets() {
    const {
      networkStatus,
      wallets,
      walletSettings,
      hardwareWallets,
    } = this.stores;
    const { hardwareWalletsConnectionData } = hardwareWallets;
    const shelleyWallets = (0, walletSorting_1.sortWallets)({
      wallets: wallets.all.filter((w) => !w.isLegacy),
      sortOrder: this.walletSortConfig.sortOrder,
      sortBy: this.walletSortConfig.sortBy,
    });
    const byronWallets = (0, walletSorting_1.sortWallets)({
      wallets: wallets.all.filter((w) => w.isLegacy),
      sortOrder: this.walletSortConfig.sortOrder,
      sortBy: this.walletSortConfig.sortBy,
    });
    const sortedWallets = [...shelleyWallets, ...byronWallets];
    return sortedWallets.map((wallet) => {
      const isHardwareWalletDisconnected = (0, lodash_1.get)(
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
  onChangeWalletSortType = (sortBy) => {
    this.walletSortConfig = (0, walletSorting_1.changeWalletSorting)({
      sortBy,
      sortOrder: this.walletSortConfig.sortOrder,
      currentSortBy: this.walletSortConfig.sortBy,
    });
    this.analytics.sendEvent(
      analytics_1.EventCategories.LAYOUT,
      'Changed wallet sorting settings'
    );
  };
  onSearchValueUpdated = (searchValue) => {
    this.searchValue = searchValue;
    this._sendSearchAnalyticsEvent();
  };
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
    } = sidebarConfig_1.sidebarConfig;
    const categoryValidation = {
      [categories.WALLETS.name]: true,
      [categories.PAPER_WALLET_CREATE_CERTIFICATE.name]: false,
      [categories.STAKING_DELEGATION_COUNTDOWN.name]: false,
      [categories.STAKING.name]: true,
      [categories.SETTINGS.name]: true,
      [categories.VOTING.name]:
        isMainnet || isDev || environment.votingVisibleOverride,
      [categories.NETWORK_INFO.name]: isFlight,
    };
    const categoriesFilteredList = list.filter(({ name }) => {
      let validator = categoryValidation[name];
      if (typeof validator === 'function') {
        validator = validator();
      }
      // @ts-ignore ts-migrate(2322) FIXME: Type 'boolean | ((...args: any[]) => any)' is not ... Remove this comment to see the full error message
      return validator;
    });
    this.CATEGORIES = categoriesFilteredList;
  };
  _onActivateSidebarCategory = (params) => {
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
  _onWalletSelected = ({ walletId }) => {
    this.stores.wallets.goToWalletRoute(walletId);
  };
  _setActivateSidebarCategory = (category) => {
    this.activeSidebarCategory = category;
  };
  _resetActivateSidebarCategory = () => {
    this.activeSidebarCategory = '';
  };
  _showSubMenus = () => {
    this.isShowingSubMenus = true;
    this.analytics.sendEvent(
      analytics_1.EventCategories.LAYOUT,
      'Toggled submenu'
    );
  };
  _hideSubMenus = () => {
    this.isShowingSubMenus = false;
    this.analytics.sendEvent(
      analytics_1.EventCategories.LAYOUT,
      'Toggled submenu'
    );
  };
  _toggleSubMenus = () => {
    this.isShowingSubMenus = !this.isShowingSubMenus;
    this.analytics.sendEvent(
      analytics_1.EventCategories.LAYOUT,
      'Toggled submenu'
    );
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
  _sendSearchAnalyticsEvent = (0, lodash_1.debounce)(() => {
    this.analytics.sendEvent(
      analytics_1.EventCategories.LAYOUT,
      'Used wallet search'
    );
  }, 5000);
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  SidebarStore.prototype,
  'CATEGORIES',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  SidebarStore.prototype,
  'activeSidebarCategory',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  SidebarStore.prototype,
  'isShowingSubMenus',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  SidebarStore.prototype,
  'walletSortConfig',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  SidebarStore.prototype,
  'searchValue',
  void 0
);
__decorate(
  [
    mobx_1.computed.struct,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  SidebarStore.prototype,
  'wallets',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  'onChangeWalletSortType',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  'onSearchValueUpdated',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  '_configureCategories',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  '_onActivateSidebarCategory',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  '_onWalletSelected',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  '_setActivateSidebarCategory',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  '_resetActivateSidebarCategory',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  '_showSubMenus',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  '_hideSubMenus',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  SidebarStore.prototype,
  '_toggleSubMenus',
  void 0
);
exports.default = SidebarStore;
//# sourceMappingURL=SidebarStore.js.map
