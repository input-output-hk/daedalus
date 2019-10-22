// @flow
import { find } from 'lodash';
import { observable, computed, action } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/LocalizedCachedRequest';
import WalletAddress from '../domains/WalletAddress';
import type { GetAddressesResponse } from '../api/addresses/types';

export default class AddressesStore extends Store {
  @observable addressesRequests: Array<{
    walletId: string,
    allRequest: CachedRequest<GetAddressesResponse>,
  }> = [];

  @computed get all(): Array<WalletAddress> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const results = this._getAddressesAllRequest(wallet.id).result;
    return results ? results.addresses : [];
  }

  @computed get hasAny(): boolean {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const results = this._getAddressesAllRequest(wallet.id).result;
    return results ? results.addresses.length > 0 : false;
  }

  @computed get active(): ?WalletAddress {
    const wallet = this.stores.wallets.active;
    if (!wallet) return null;
    const results = this._getAddressesAllRequest(wallet.id).result;
    return results ? results.addresses[results.addresses.length - 1] : null;
  }

  @computed get totalAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const results = this._getAddressesAllRequest(wallet.id).result;
    return results ? results.addresses.length : 0;
  }

  @action _refreshAddresses = () => {
    if (this.stores.networkStatus.isConnected) {
      const allWallets = this.stores.wallets.all;
      for (const wallet of allWallets) {
        const allRequest = this._getAddressesAllRequest(wallet.id);
        allRequest.invalidate({ immediately: false });
        allRequest.execute({ walletId: wallet.id });
      }
    }
  };

  getAddressesByWalletId = async (
    walletId: string
  ): Promise<Array<WalletAddress>> => {
    const result = await this._getAddressesAllRequest(walletId);
    return result ? result.addresses : [];
  };

  _getAddressesAllRequest = (
    walletId: string
  ): CachedRequest<GetAddressesResponse> => {
    const foundRequest = find(this.addressesRequests, { walletId });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new CachedRequest(this.api.ada.getAddresses);
  };
}
