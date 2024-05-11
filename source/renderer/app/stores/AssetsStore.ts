import { observable, action, computed, makeObservable } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Asset from '../domains/Asset';
import { ROUTES } from '../routes-config';
import { ellipsis } from '../utils/strings';
import type { GetAssetsResponse, AssetToken } from '../api/assets/types';
import { AnalyticsTracker, EventCategories } from '../analytics';
import { Api } from '../api';
import { ActionsMap } from '../actions';

type WalletId = string;
export default class AssetsStore extends Store {
  ASSETS_REFRESH_INTERVAL: number = 1 * 60 * 1000; // 1 minute | unit: milliseconds

  // REQUESTS
  favoritesRequest: Request<Record<string, any>> = new Request(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'AssetsStore... Remove this comment to see the full error message
    this.api.localStorage.getWalletTokenFavorites
  );
  activeAsset: string | null | undefined = null;
  editedAsset: AssetToken | null | undefined = null;
  assetsRequests: Record<WalletId, Request<GetAssetsResponse>> = {};
  insertingAssetUniqueId: string | null | undefined = null;
  removingAssetUniqueId: string | null | undefined = null;

  constructor(api: Api, actions: ActionsMap, analytics: AnalyticsTracker) {
    super(api, actions, analytics);

    makeObservable(this, {
      favoritesRequest: observable,
      activeAsset: observable,
      editedAsset: observable,
      assetsRequests: observable,
      insertingAssetUniqueId: observable,
      removingAssetUniqueId: observable,
      all: computed,
      details: computed,
      favorites: computed,
      _onEditedAssetSet: action,
      _onAssetSettingsSubmit: action,
      _onEditedAssetUnset: action,
      _onOpenAssetSend: action,
      _onCopyAssetParam: action,
      _refreshAssetsData: action,
      _setActiveAsset: action,
      _unsetActiveAsset: action,
      _createWalletTokensRequest: action,
      _onToggleFavorite: action,
    });
  }

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
  get all(): Array<Asset> {
    const wallet = this.stores.wallets.active;

    if (!wallet) {
      return [];
    }

    const request = this._retrieveAssetsRequest(wallet.id);

    return get(request, 'result.assets', []);
  }

  get details(): Record<string, Asset> {
    return this.all.reduce((details, asset) => {
      const { policyId, assetName } = asset;
      details[`${policyId}${assetName}`] = asset;
      return details;
    }, {});
  }

  getAsset = (policyId: string, assetName: string): Asset | null | undefined =>
    this.details[`${policyId}${assetName}`];

  get favorites(): Record<string, any> {
    return this.favoritesRequest.result || {};
  }

  // =================== PRIVATE ==================
  _setUpFavorites = async () => {
    this.favoritesRequest.execute();
  };
  _onEditedAssetSet = ({ asset }: { asset: AssetToken }) => {
    this.editedAsset = asset;
  };
  _onAssetSettingsSubmit = async ({
    asset,
    decimals,
  }: {
    asset: AssetToken;
    decimals: number;
  }) => {
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
      EventCategories.WALLETS,
      'Changed native token settings'
    );
  };
  _onEditedAssetUnset = () => {
    this.editedAsset = null;
  };
  _onOpenAssetSend = ({ uniqueId }: { uniqueId: string }) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'AssetsSt... Remove this comment to see the full error message
    const { stores, actions } = this;
    const { wallets } = stores;
    const { active } = wallets;

    if (active) {
      const { id } = active;
      const { wallets: walletActions, router } = actions;
      walletActions.setActiveAsset.trigger(uniqueId);
      router.goToRoute.trigger({
        route: ROUTES.WALLETS.PAGE,
        params: {
          id,
          page: 'send',
        },
      });
    }
  };
  _onCopyAssetParam = ({
    param,
    fullValue,
  }: {
    param: string;
    fullValue: string;
  }) => {
    const shortValue = ellipsis(fullValue, 15, 15);
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
  _setActiveAsset = (uniqueId: string) => {
    this.activeAsset = uniqueId;
  };
  _unsetActiveAsset = () => {
    this.activeAsset = null;
  };
  _createWalletTokensRequest = (
    walletId: string
  ): Request<GetAssetsResponse> => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'AssetsStore... Remove this comment to see the full error message
    this.assetsRequests[walletId] = new Request(this.api.ada.getAssets);
    return this.assetsRequests[walletId];
  };
  _onToggleFavorite = async ({
    uniqueId,
    isFavorite,
  }: {
    uniqueId: string;
    isFavorite: boolean;
  }) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'AssetsStore... Remove this comment to see the full error message
    await this.api.localStorage.toggleWalletTokenFavorite(
      uniqueId,
      !isFavorite
    );
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.favoritesRequest.execute();

    this.analytics.sendEvent(
      EventCategories.WALLETS,
      `${!isFavorite ? 'Added token to' : 'Removed token from'} favorites`
    );
  };
  _retrieveAssetsRequest = (walletId: string): Request<GetAssetsResponse> =>
    this.assetsRequests[walletId] || this._createWalletTokensRequest(walletId);
}
