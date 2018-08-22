// @flow
import { observable, computed, action, runInAction } from 'mobx';
import _ from 'lodash';
import Store from '../lib/Store';
import CachedRequest from '../lib/LocalizedCachedRequest';
import Request from '../lib/LocalizedRequest';
import type { AdaAddress } from '../../api/ada/types';
import LocalizableError from '../../i18n/LocalizableError';
import type { GetAddressesResponse, CreateAddressResponse } from '../../api/ada/index';

export default class AddressesStore extends Store {

  @observable lastGeneratedAddress: ?AdaAddress = null;
  @observable addressesRequests: Array<{
    walletId: string,
    allRequest: CachedRequest<GetAddressesResponse>
  }> = [];
  @observable error: ?LocalizableError = null;

  // REQUESTS
  /* eslint-disable max-len */
  @observable createAddressRequest: Request<CreateAddressResponse> = new Request(this.api.ada.createAddress);
  /* eslint-disable max-len */

  setup() {
    const actions = this.actions.ada.addresses;
    actions.createAddress.listen(this._createAddress);
    actions.resetErrors.listen(this._resetErrors);
  }

  _createAddress = async (params: { walletId: string, spendingPassword?: string }) => {
    try {
      const { walletId, spendingPassword } = params;
      const accountIndex = await this.getAccountIdByWalletId(walletId);

      const address: ?CreateAddressResponse = await this.createAddressRequest.execute({
        accountIndex, spendingPassword, walletId
      }).promise;

      if (address != null) {
        this._refreshAddresses();
        runInAction('set last generated address and reset error', () => {
          this.lastGeneratedAddress = address;
          this.error = null;
        });
      }
    } catch (error) {
      runInAction('set error', () => { this.error = error; });
    }
  };

  @computed get all(): Array<AdaAddress> {
    const wallet = this.stores.ada.wallets.active;
    if (!wallet) return [];
    const result = this._getAddressesAllRequest(wallet.id).result;
    return result ? result.addresses : [];
  }

  @computed get hasAny(): boolean {
    const wallet = this.stores.ada.wallets.active;
    if (!wallet) return false;
    const result = this._getAddressesAllRequest(wallet.id).result;
    return result ? result.addresses.length > 0 : false;
  }

  @computed get active(): ?AdaAddress {
    if (this.lastGeneratedAddress) return this.lastGeneratedAddress;
    const wallet = this.stores.ada.wallets.active;
    if (!wallet) return;
    const result = this._getAddressesAllRequest(wallet.id).result;
    return result ? result.addresses[result.addresses.length - 1] : null;
  }

  @computed get totalAvailable(): number {
    const wallet = this.stores.ada.wallets.active;
    if (!wallet) return 0;
    const result = this._getAddressesAllRequest(wallet.id).result;
    return result ? result.addresses.length : 0;
  }

  @action _refreshAddresses = () => {
    if (this.stores.networkStatus.isConnected) {
      const allWallets = this.stores.ada.wallets.all;
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

  getAccountIdByWalletId = async (walletId: string): Promise<?number> => {
    const { accountIndex } = await this._getAddressesAllRequest(walletId);
    return accountIndex || null;
  };

  getAddressesByWalletId = async (walletId: string): Promise<Array<string>> => {
    const result = await this._getAddressesAllRequest(walletId);
    return result ? result.addresses : [];
  };

  _getAddressesAllRequest = (walletId: string): CachedRequest<GetAddressesResponse> => {
    const foundRequest = _.find(this.addressesRequests, { walletId });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new CachedRequest(this.api.ada.getAddresses);
  };

}
