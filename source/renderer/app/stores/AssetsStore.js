// @flow
import { observable, action, computed } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Asset from '../domains/Asset';
import { ROUTES } from '../routes-config';
import { requestGetter } from '../utils/storesUtils';
import { ellipsis } from '../utils/strings';
import type { GetAssetsResponse, AssetToken } from '../api/assets/types';
import { TOGGLE_TOKEN_FAVORITE_TIMEOUT } from '../config/timingConfig';

type WalletId = string;

export default class AssetsStore extends Store {
  ASSETS_REFRESH_INTERVAL: number = 1 * 60 * 1000; // 1 minute | unit: milliseconds

  // REQUESTS
  @observable favoritesRequest: Request<Object> = new Request(
    this.api.localStorage.getWalletTokenFavorites
  );

  @observable activeAsset: ?string = null;
  @observable editingsAsset: ?AssetToken = null;
  @observable assetsRequests: {
    [key: WalletId]: Request<GetAssetsResponse>,
  } = {};
  @observable insertingAssetUniqueId: ?string = null;
  @observable removingAssetUniqueId: ?string = null;

  // REQUESTS
  @observable
  getAssetSettingsDialogWasOpenedRequest: Request<void> = new Request(
    this.api.localStorage.getAssetSettingsDialogWasOpened
  );

  setup() {
    setInterval(this._refreshAssetsData, this.ASSETS_REFRESH_INTERVAL);
    const { assets: assetsActions, wallets: walletsActions } = this.actions;
    assetsActions.onAssetSettingsOpen.listen(this._onAssetSettingsOpen);
    assetsActions.onAssetSettingsSubmit.listen(this._onAssetSettingsSubmit);
    assetsActions.onAssetSettingsCancel.listen(this._onAssetSettingsCancel);
    assetsActions.onOpenAssetSend.listen(this._onOpenAssetSend);
    assetsActions.onCopyAssetParam.listen(this._onCopyAssetParam);
    assetsActions.onToggleFavorite.listen(this._onToggleFavorite);

    walletsActions.refreshWalletsDataSuccess.once(this._refreshAssetsData);
    walletsActions.setActiveAsset.listen(this._setActiveAsset);
    walletsActions.unsetActiveAsset.listen(this._unsetActiveAsset);

    this._setUpFavorites();
  }

  // ==================== PUBLIC ==================

  @computed get all(): Array<Asset> {
    const wallet = this.stores.wallets.active;
    if (!wallet) {
      return [];
    }
    const request = this._retrieveAssetsRequest(wallet.id);
    return get(request, 'result.assets', []);
  }

  @computed get details(): {
    [key: string]: Asset,
  } {
    return this.all.reduce((details, asset) => {
      const { policyId, assetName } = asset;
      details[`${policyId}${assetName}`] = asset;
      return details;
    }, {});
  }

  getAsset = (policyId: string, assetName: string): ?Asset =>
    this.details[`${policyId}${assetName}`];

  @computed get assetSettingsDialogWasOpened(): boolean {
    return requestGetter(this.getAssetSettingsDialogWasOpenedRequest, false);
  }

  @computed get favorites(): Object {
    return this.favoritesRequest.result || {};
  }

  // =================== PRIVATE ==================

  _setUpFavorites = async () => {
    this.favoritesRequest.execute();
  };

  @action _onAssetSettingsOpen = ({ asset }: { asset: AssetToken }) => {
    this.editingsAsset = asset;
    this.api.localStorage.setAssetSettingsDialogWasOpened();
    this.getAssetSettingsDialogWasOpenedRequest.execute();
  };

  @action _onAssetSettingsSubmit = async ({
    asset,
    decimals,
  }: {
    asset: AssetToken,
    decimals: number,
  }) => {
    this.editingsAsset = null;
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
  };

  @action _onAssetSettingsCancel = () => {
    this.editingsAsset = null;
  };

  @action _onOpenAssetSend = ({ uniqueId }: { uniqueId: string }) => {
    const { stores, actions } = this;
    const { wallets } = stores;
    const { active } = wallets;
    if (active) {
      const { id } = active;
      const { wallets: walletActions, router } = actions;
      walletActions.setActiveAsset.trigger(uniqueId);
      router.goToRoute.trigger({
        route: ROUTES.WALLETS.PAGE,
        params: { id, page: 'send' },
      });
    }
  };

  @action _onCopyAssetParam = ({
    param,
    fullValue,
  }: {
    param: string,
    fullValue: string,
  }) => {
    const shortValue = ellipsis(fullValue, 15, 15);
    this.actions.assets.copyAssetParamNotification.trigger({
      param,
      shortValue,
    });
  };

  @action _refreshAssetsData = () => {
    if (this.stores.networkStatus.isConnected) {
      const { all } = this.stores.wallets;
      for (const wallet of all) {
        const { id: walletId } = wallet;
        this._retrieveAssetsRequest(walletId).execute({ walletId });
      }
    }
  };

  @action _setActiveAsset = (uniqueId: string) => {
    this.activeAsset = uniqueId;
  };

  @action _unsetActiveAsset = () => {
    this.activeAsset = null;
  };

  @action _createWalletTokensRequest = (
    walletId: string
  ): Request<GetAssetsResponse> => {
    this.assetsRequests[walletId] = new Request(this.api.ada.getAssets);
    return this.assetsRequests[walletId];
  };

  @action updateFavoriteLocalStorage = async (
    uniqueId: string,
    isFavorite: boolean
  ) => {
    await this.api.localStorage.toggleWalletTokenFavorite(uniqueId, isFavorite);
    await this.favoritesRequest.execute();
  };

  @action clearFavoriteAction = () => {
    this.insertingAssetUniqueId = null;
    this.removingAssetUniqueId = null;
  };

  /**
   *
   * This function toggles a tokens favorite item
   * It also adds for half a second the token id to
   * either `removingAssetUniqueId` or `insertingAssetUniqueId`
   * so the item can be animated.
   * This is important, because the whole list shifts up or down.
   *
   */
  @action _onToggleFavorite = async ({
    uniqueId,
    isFavorite,
  }: {
    uniqueId: string,
    isFavorite: boolean,
  }) => {
    if (this.insertingAssetUniqueId || this.removingAssetUniqueId) {
      return;
    }
    if (isFavorite) {
      // It's removing favorite
      // We need to wait for the element to be removed, before updating the favorites list
      this.removingAssetUniqueId = uniqueId;
      setTimeout(async () => {
        await this.updateFavoriteLocalStorage(uniqueId, false);
        this.clearFavoriteAction();
      }, TOGGLE_TOKEN_FAVORITE_TIMEOUT);
    } else {
      // It's inserting favorite
      // We update the favorites list straight away
      this.insertingAssetUniqueId = uniqueId;
      await this.updateFavoriteLocalStorage(uniqueId, true);
      setTimeout(() => {
        this.clearFavoriteAction();
      }, TOGGLE_TOKEN_FAVORITE_TIMEOUT);
    }
  };

  _retrieveAssetsRequest = (walletId: string): Request<GetAssetsResponse> =>
    this.assetsRequests[walletId] || this._createWalletTokensRequest(walletId);
}
