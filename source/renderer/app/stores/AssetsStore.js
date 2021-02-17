// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import type { GetAssetsResponse } from '../api/assets/types';
import Asset from '../domains/Asset';

type WalletId = string;

export default class AssetsStore extends Store {
  ASSETS_REFRESH_INTERVAL: number = 1 * 60 * 1000; // 1 second | unit: milliseconds

  @observable assetsRequests: {
    [key: WalletId]: Request<GetAssetsResponse>,
  } = {};

  setup() {
    setInterval(this._refreshAssetsData, this.ASSETS_REFRESH_INTERVAL);
    const { wallets: walletsActions } = this.actions;
    walletsActions.refreshWalletsDataSuccess.once(this._refreshAssetsData);
  }

  // ==================== PUBLIC ==================

  @computed get all(): Array<Asset> {
    const wallet = this.stores.wallets.active;
    if (!wallet) {
      return [];
    }
    const request = this._getWalletAssetsRequest(wallet.id);
    if (!request.result) {
      return [];
    }
    return request.result.assets || [];
  }

  // =================== PRIVATE ==================

  @action _refreshAssetsData = () => {
    if (this.stores.networkStatus.isConnected) {
      const { all } = this.stores.wallets;
      for (const wallet of all) {
        this._createWalletAssetsRequest(wallet.id);
      }
    }
  };

  @action _createWalletAssetsRequest = (
    walletId: string
  ): Request<GetAssetsResponse> => {
    this.assetsRequests[walletId] = new Request(this.api.ada.getAssets);
    this.assetsRequests[walletId].execute({ walletId });
    return this.assetsRequests[walletId];
  };

  _getWalletAssetsRequest = (walletId: string): Request<GetAssetsResponse> =>
    this.assetsRequests[walletId] || this._createWalletAssetsRequest(walletId);
}
