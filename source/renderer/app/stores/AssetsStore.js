// @flow
import { observable, computed, action } from 'mobx';
import { find } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import type {
  GetAssetRequest,
  GetAssetsResponse,
  WalletAssetItem,
} from '../api/assets/types';
import Asset from '../domains/Asset';

export default class AssetsStore extends Store {
  @observable assetsRequests: Array<{
    walletId: string,
    allRequest: Request<GetAssetsResponse>,
  }> = [];

  @observable
  getSingleAssetRequest: Request<GetAssetRequest> = new Request(
    this.api.ada.getAsset
  );

  setup() {}

  @computed get all(): Array<Asset> {
    const wallet = this.stores.wallets.active;
    if (!wallet) {
      return [];
    }
    const request = this._getAssetsAllRequest(wallet.id);
    if (!request.result) {
      return [];
    }
    return request.result.assets || [];
  }

  @computed get totalAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const results = this._getAssetsAllRequest(wallet.id).result;
    return results ? results.total : 0;
  }

  @action _refreshAssets = () => {
    if (this.stores.networkStatus.isConnected) {
      const { all } = this.stores.wallets;
      for (const wallet of all) {
        const { id: walletId } = wallet;
        const allRequest = this._getAssetsAllRequest(walletId);
        allRequest.execute({ walletId });
      }
    }
  };

  _getAssetsAllRequest = (walletId: string): Request<GetAssetsResponse> => {
    const foundRequest = find(this.assetsRequests, { walletId });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new Request(this.api.ada.getAssets);
  };

  _getAssetFingerprint = (asset: WalletAssetItem): string => {
    const assetDetail =
      this.all.find(
        ({ policyId, assetName }) =>
          asset.policyId === policyId && asset.assetName === assetName
      ) || {};
    return assetDetail.fingerprint;
  };

  getSingleAsset = async ({
    walletId,
    policyId,
    assetName,
  }: {
    walletId: string,
    policyId: string,
    assetName: string,
  }) => {
    const wallet = this.stores.wallets.getWalletById(walletId);
    if (!wallet) {
      throw new Error('Active wallet required before fetching single asset.');
    }
    await this.getSingleAssetRequest.execute({
      walletId,
      policyId,
      assetName,
    });
  };
}
