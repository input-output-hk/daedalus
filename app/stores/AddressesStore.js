// @flow
import { observable, computed, action, extendObservable } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import CachedRequest from './lib/LocalizedCachedRequest';
import WalletAddress from '../domain/WalletAddress';
import type { GetTransactionsResponse } from '../api';

export default class AddressesStore extends Store {

  @observable addressesRequests: Array<{
    walletId: string,
    allRequest: CachedRequest<GetTransactionsResponse>
  }> = [];

  setup() {
    const actions = this.actions.addresses;
  }

  @computed get all(): Array<WalletAddress> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const result = this._getAddressesAllRequest(wallet.id).result;
    return result ? result.addresses : [];
  }

  @computed get hasAny(): boolean {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const result = this._getAddressesAllRequest(wallet.id).result;
    return result ? result.addresses.length > 0 : false;
  }

  @computed get active(): ?WalletAddress {
    const wallet = this.stores.wallets.active;
    if (!wallet) return;
    const result = this._getAddressesAllRequest(wallet.id).result;
    return result ? result.addresses[result.addresses.length - 1] : null;
    // TODO: When user generates new address that one should be active
  }

  @computed get totalAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const result = this._getAddressesAllRequest(wallet.id).result;
    return result ? result.addresses.length : 0;
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

  _getAddressesAllRequest = (walletId: string): CachedRequest<GetTransactionsResponse> => {
    const foundRequest = _.find(this.addressesRequests, { walletId });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new CachedRequest(this.api.getAddresses);
  };

}
