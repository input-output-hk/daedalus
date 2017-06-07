// @flow
import { observable, computed, action } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';
import CachedRequest from './lib/LocalizedCachedRequest';
import Request from './lib/LocalizedRequest';
import WalletAddress from '../domain/WalletAddress';
import type {
  GetAddressesResponse,
  CreateAddressResponse,
} from '../api';

export default class AddressesStore extends Store {

  @observable addressesRequests: Array<{
    walletId: string,
    allRequest: CachedRequest<GetAddressesResponse>
  }> = [];

  // REQUESTS
  /* eslint-disable max-len */
  @observable createAddressRequest: Request<CreateAddressResponse> = new Request(this.api.createAddress);
  /* eslint-disable max-len */

  setup() {
    const actions = this.actions.addresses;
    actions.createAddress.listen(this._createAddress);
  }

  _createAddress = async (params: { walletId: string, password: ?string }) => {
    try {
      const { walletId, password } = params;
      const accountId = this._getAccountIdByWalletId(walletId);
      const address: ?CreateAddressResponse = await this.createAddressRequest.execute({
        accountId, password
      }).promise;
      if (address != null) this._refreshAddresses();
    } catch (error) {
      throw error;
    }
  };

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

  _getAccountIdByWalletId = (walletId: string): ?string => {
    const result = this._getAddressesAllRequest(walletId).result;
    return result ? result.accountId : null;
  };

  _getAddressesAllRequest = (walletId: string): CachedRequest<GetAddressesResponse> => {
    const foundRequest = _.find(this.addressesRequests, { walletId });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new CachedRequest(this.api.getAddresses);
  };

}
