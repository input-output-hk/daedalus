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
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const routes_config_1 = require('../routes-config');
const strings_1 = require('../utils/strings');
const analytics_1 = require('../analytics');
class AssetsStore extends Store_1.default {
  ASSETS_REFRESH_INTERVAL = 1 * 60 * 1000; // 1 minute | unit: milliseconds
  // REQUESTS
  favoritesRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'AssetsStore... Remove this comment to see the full error message
    this.api.localStorage.getWalletTokenFavorites
  );
  activeAsset = null;
  editedAsset = null;
  assetsRequests = {};
  insertingAssetUniqueId = null;
  removingAssetUniqueId = null;
  setup() {
    setInterval(this._refreshAssetsData, this.ASSETS_REFRESH_INTERVAL);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AssetsS... Remove this comment to see the full error message
    const { assets: assetsActions, wallets: walletsActions } = this.actions;
    assetsActions.setEditedAsset.listen(this._onEditedAssetSet);
    assetsActions.onAssetSettingsSubmit.listen(this._onAssetSettingsSubmit);
    assetsActions.unsetEditedAsset.listen(this._onEditedAssetUnset);
    assetsActions.onOpenAssetSend.listen(this._onOpenAssetSend);
    assetsActions.onCopyAssetParam.listen(this._onCopyAssetParam);
    assetsActions.onToggleFavorite.listen(this._onToggleFavorite);
    walletsActions.refreshWalletsDataSuccess.once(this._refreshAssetsData);
    walletsActions.setActiveAsset.listen(this._setActiveAsset);
    walletsActions.unsetActiveAsset.listen(this._unsetActiveAsset);
    this._setUpFavorites();
  }
  // ==================== PUBLIC ==================
  get all() {
    const wallet = this.stores.wallets.active;
    if (!wallet) {
      return [];
    }
    const request = this._retrieveAssetsRequest(wallet.id);
    return (0, lodash_1.get)(request, 'result.assets', []);
  }
  get details() {
    return this.all.reduce((details, asset) => {
      const { policyId, assetName } = asset;
      details[`${policyId}${assetName}`] = asset;
      return details;
    }, {});
  }
  getAsset = (policyId, assetName) => this.details[`${policyId}${assetName}`];
  get favorites() {
    return this.favoritesRequest.result || {};
  }
  // =================== PRIVATE ==================
  _setUpFavorites = async () => {
    this.favoritesRequest.execute();
  };
  _onEditedAssetSet = ({ asset }) => {
    this.editedAsset = asset;
  };
  _onAssetSettingsSubmit = async ({ asset, decimals }) => {
    this.editedAsset = null;
    const { policyId, assetName } = asset;
    const assetDomain = this.getAsset(policyId, assetName);
    if (assetDomain) {
      assetDomain.update({
        decimals,
      });
    }
    this._refreshAssetsData();
    await this.api.localStorage.setAssetLocalData(policyId, assetName, {
      decimals,
    });
    this.analytics.sendEvent(
      analytics_1.EventCategories.WALLETS,
      'Changed native token settings'
    );
  };
  _onEditedAssetUnset = () => {
    this.editedAsset = null;
  };
  _onOpenAssetSend = ({ uniqueId }) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'AssetsSt... Remove this comment to see the full error message
    const { stores, actions } = this;
    const { wallets } = stores;
    const { active } = wallets;
    if (active) {
      const { id } = active;
      const { wallets: walletActions, router } = actions;
      walletActions.setActiveAsset.trigger(uniqueId);
      router.goToRoute.trigger({
        route: routes_config_1.ROUTES.WALLETS.PAGE,
        params: {
          id,
          page: 'send',
        },
      });
    }
  };
  _onCopyAssetParam = ({ param, fullValue }) => {
    const shortValue = (0, strings_1.ellipsis)(fullValue, 15, 15);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'AssetsS... Remove this comment to see the full error message
    this.actions.assets.copyAssetParamNotification.trigger({
      param,
      shortValue,
    });
  };
  _refreshAssetsData = () => {
    if (this.stores.networkStatus.isConnected) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'AssetsSt... Remove this comment to see the full error message
      const { all } = this.stores.wallets;
      for (const wallet of all) {
        const { id: walletId } = wallet;
        this._retrieveAssetsRequest(walletId).execute({
          walletId,
        });
      }
    }
  };
  _setActiveAsset = (uniqueId) => {
    this.activeAsset = uniqueId;
  };
  _unsetActiveAsset = () => {
    this.activeAsset = null;
  };
  _createWalletTokensRequest = (walletId) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'AssetsStore... Remove this comment to see the full error message
    this.assetsRequests[walletId] = new LocalizedRequest_1.default(
      this.api.ada.getAssets
    );
    return this.assetsRequests[walletId];
  };
  _onToggleFavorite = async ({ uniqueId, isFavorite }) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'AssetsStore... Remove this comment to see the full error message
    await this.api.localStorage.toggleWalletTokenFavorite(
      uniqueId,
      !isFavorite
    );
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.favoritesRequest.execute();
    this.analytics.sendEvent(
      analytics_1.EventCategories.WALLETS,
      `${!isFavorite ? 'Added token to' : 'Removed token from'} favorites`
    );
  };
  _retrieveAssetsRequest = (walletId) =>
    this.assetsRequests[walletId] || this._createWalletTokensRequest(walletId);
}
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  AssetsStore.prototype,
  'favoritesRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  AssetsStore.prototype,
  'activeAsset',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AssetsStore.prototype,
  'editedAsset',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AssetsStore.prototype,
  'assetsRequests',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  AssetsStore.prototype,
  'insertingAssetUniqueId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  AssetsStore.prototype,
  'removingAssetUniqueId',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  AssetsStore.prototype,
  'all',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  AssetsStore.prototype,
  'details',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  AssetsStore.prototype,
  'favorites',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_onEditedAssetSet',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_onAssetSettingsSubmit',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_onEditedAssetUnset',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_onOpenAssetSend',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_onCopyAssetParam',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_refreshAssetsData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_setActiveAsset',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_unsetActiveAsset',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_createWalletTokensRequest',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AssetsStore.prototype,
  '_onToggleFavorite',
  void 0
);
exports.default = AssetsStore;
//# sourceMappingURL=AssetsStore.js.map
