// @flow
import { observable, action, computed } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import Asset from '../domains/Asset';
import type { GetAssetsResponse } from '../api/assets/types';

type WalletId = string;

export default class AssetsStore extends Store {
  ASSETS_REFRESH_INTERVAL: number = 1 * 60 * 1000; // 1 minute | unit: milliseconds

  @observable activeAssetFingerprint: ?string = null;

  @observable assetsRequests: {
    [key: WalletId]: Request<GetAssetsResponse>,
  } = {};

  setup() {
    setInterval(this._refreshAssetsData, this.ASSETS_REFRESH_INTERVAL);
    const { wallets: walletsActions } = this.actions;
    walletsActions.refreshWalletsDataSuccess.once(this._refreshAssetsData);
    walletsActions.setActiveAssetFingerprint.listen(
      this._setActiveAssetFingerprint
    );
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

  // =================== PRIVATE ==================

  @action _refreshAssetsData = () => {
    if (this.stores.networkStatus.isConnected) {
      const { all } = this.stores.wallets;
      for (const wallet of all) {
        const { id: walletId } = wallet;
        this._retrieveAssetsRequest(walletId).execute({ walletId });
      }
    }
  };

  @action _setActiveAssetFingerprint = (params: { fingerprint: ?string }) => {
    this.activeAssetFingerprint = params.fingerprint;
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
