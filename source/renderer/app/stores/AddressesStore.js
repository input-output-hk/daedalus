// @flow
import { find, last } from 'lodash';
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/LocalizedCachedRequest';
import WalletAddress from '../domains/WalletAddress';

export default class AddressesStore extends Store {
  @observable addressesRequests: Array<{
    walletId: string,
    isLegacy: boolean,
    allRequest: CachedRequest<Array<WalletAddress>>,
  }> = [];

  @computed get all(): Array<WalletAddress> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const addresses = this._getAddressesAllRequest(wallet.id).result;
    return addresses || [];
  }

  @computed get hasAny(): boolean {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const addresses = this._getAddressesAllRequest(wallet.id).result;
    return addresses ? addresses.length > 0 : false;
  }

  @computed get active(): ?WalletAddress {
    const wallet = this.stores.wallets.active;
    if (!wallet) return null;
    const addresses = this._getAddressesAllRequest(wallet.id).result;
    return addresses ? last(addresses) : null;
  }

  @computed get totalAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const addresses = this._getAddressesAllRequest(wallet.id).result;
    return addresses ? addresses.length : 0;
  }

  @action _refreshAddresses = () => {
    if (this.stores.networkStatus.isConnected) {
      const { all } = this.stores.wallets;
      for (const wallet of all) {
        const allRequest = this._getAddressesAllRequest(wallet.id);
        allRequest.invalidate({ immediately: false });
        allRequest.execute({ walletId: wallet.id, isLegacy: wallet.isLegacy });
      }
    }
  };

  getAddressesByWalletId = async (
    walletId: string
  ): Promise<Array<WalletAddress>> => {
    const addresses = await this._getAddressesAllRequest(walletId);
    return addresses || [];
  };

  _getAddressesAllRequest = (
    walletId: string
  ): CachedRequest<Array<WalletAddress>> => {
    const foundRequest = find(this.addressesRequests, { walletId });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new CachedRequest(this.api.ada.getAddresses);
  };
}
