// @flow
import { observable, action, computed } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Asset from '../domains/Asset';
import { requestGetter } from '../utils/storesUtils';
import type {
  GetAssetsResponse,
  WalletSummaryAsset,
} from '../api/assets/types';

type WalletId = string;

export default class AssetsStore extends Store {
  ASSETS_REFRESH_INTERVAL: number = 1 * 60 * 1000; // 1 minute | unit: milliseconds

  // @FINGERPRINT TODO
  @observable activeAsset: ?string = null;
  @observable editingsAsset: ?WalletSummaryAsset = null;
  @observable assetsRequests: {
    [key: WalletId]: Request<GetAssetsResponse>,
  } = {};

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

    walletsActions.refreshWalletsDataSuccess.once(this._refreshAssetsData);
    walletsActions.setActiveAsset.listen(this._setActiveAsset);
    walletsActions.unsetActiveAsset.listen(this._unsetActiveAsset);
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
      details[policyId + assetName] = asset;
      return details;
    }, {});
  }

  getAssetDetails = (policyId: string, assetName: string): ?Asset =>
    this.details[policyId + assetName];

  @computed get assetSettingsDialogWasOpened(): boolean {
    return requestGetter(this.getAssetSettingsDialogWasOpenedRequest, false);
  }

  // =================== PRIVATE ==================

  @action _onAssetSettingsOpen = ({ asset }: { asset: WalletSummaryAsset }) => {
    this.editingsAsset = asset;
    this.api.localStorage.setAssetSettingsDialogWasOpened();
    this.getAssetSettingsDialogWasOpenedRequest.execute();
  };

  @action _onAssetSettingsSubmit = async ({
    asset,
    decimals,
  }: {
    asset: WalletSummaryAsset,
    decimals: number,
  }) => {
    this.editingsAsset = null;
    const { policyId, assetName } = asset;
    const assetDomain = this.getAssetDetails(policyId, assetName);
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

  @action _createWalletAssetsRequest = (
    walletId: string
  ): Request<GetAssetsResponse> => {
    this.assetsRequests[walletId] = new Request(this.api.ada.getAssets);
    return this.assetsRequests[walletId];
  };

  _retrieveAssetsRequest = (walletId: string): Request<GetAssetsResponse> =>
    this.assetsRequests[walletId] || this._createWalletAssetsRequest(walletId);
}
