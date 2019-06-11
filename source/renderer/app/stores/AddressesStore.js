// @flow
import { find } from 'lodash';
import { observable, computed, action, runInAction } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/LocalizedCachedRequest';
import Request from './lib/LocalizedRequest';
import LocalizableError from '../i18n/LocalizableError';
import type {
  Address,
  Addresses,
  GetAddressesResponse,
} from '../api/addresses/types';

export default class AddressesStore extends Store {
  @observable lastGeneratedAddress: ?Address = null;
  @observable addressesRequests: Array<{
    walletId: string,
    allRequest: CachedRequest<GetAddressesResponse>,
  }> = [];
  @observable error: ?LocalizableError = null;

  // REQUESTS
  /* eslint-disable max-len */
  @observable createAddressRequest: Request<Address> = new Request(
    this.api.ada.createAddress
  );
  /* eslint-disable max-len */

  setup() {
    const actions = this.actions.addresses;
    actions.createAddress.listen(this._createAddress);
    actions.resetErrors.listen(this._resetErrors);
  }

  _createAddress = async (params: {
    walletId: string,
    spendingPassword: ?string,
  }) => {
    try {
      const { walletId, spendingPassword } = params;
      const accountIndex = await this.getAccountIndexByWalletId(walletId);

      const address: ?Address = await this.createAddressRequest.execute({
        accountIndex,
        spendingPassword,
        walletId,
      }).promise;

      if (address != null) {
        this._refreshAddresses();
        runInAction('set last generated address and reset error', () => {
          this.lastGeneratedAddress = address;
          this.error = null;
        });
      }
    } catch (error) {
      runInAction('set error', () => {
        this.error = error;
      });
    }
  };

  @computed get all(): Addresses {
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

  @computed get active(): ?Address {
    if (this.lastGeneratedAddress) return this.lastGeneratedAddress;
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

  @action _resetErrors = () => {
    this.error = null;
  };

  getAccountIndexByWalletId = async (walletId: string): Promise<?number> => {
    const result = await this.api.ada.getAddresses({ walletId });
    return result ? result.accountIndex : null;
  };

  getAddressesByWalletId = async (walletId: string): Promise<Array<string>> => {
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
