// @flow
import { find, last } from 'lodash';
import { observable, computed, action, runInAction } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/LocalizedCachedRequest';
import WalletAddress from '../domains/WalletAddress';
import Request from './lib/LocalizedRequest';
import LocalizableError from '../i18n/LocalizableError';
import { GenericApiError } from '../api/common/errors';
import type { Address } from '../api/addresses/types';

export default class AddressesStore extends Store {
  @observable lastGeneratedAddress: ?Address = null;
  @observable addressesRequests: Array<{
    walletId: string,
    isLegacy: boolean,
    allRequest: CachedRequest<Array<WalletAddress>>,
  }> = [];
  @observable error: ?LocalizableError = null;

  // REQUESTS
  /* eslint-disable max-len */
  @observable createByronWalletAddressRequest: Request<Address> = new Request(
    this.api.ada.createAddress
  );
  /* eslint-disable max-len */

  setup() {
    const actions = this.actions.addresses;
    actions.createByronWalletAddress.listen(this._createByronWalletAddress);
    actions.resetErrors.listen(this._resetErrors);
  }

  _createByronWalletAddress = async (params: {
    walletId: string,
    spendingPassword: ?string,
  }) => {
    try {
      const { walletId, spendingPassword } = params;
      const accountIndex = await this.getAccountIndexByWalletId(walletId);

      const address: ?Address = await this.createByronWalletAddressRequest.execute(
        {
          accountIndex,
          spendingPassword,
          walletId,
        }
      ).promise;

      if (address != null) {
        this._refreshAddresses();
        runInAction('set last generated address and reset error', () => {
          this.lastGeneratedAddress = address;
          this.error = null;
        });
      }
    } catch (error) {
      runInAction('set error', () => {
        // @TODO - Pass real error from api response once api endpoint is integrated
        // this.error = error;
        this.error = new GenericApiError();
      });
    }
  };

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

  @action _resetErrors = () => {
    this.error = null;
  };

  getAccountIndexByWalletId = async (walletId: string): Promise<?number> => {
    // $FlowFixMe
    const result = await this.api.ada.getAddresses({ walletId });
    return result ? result.accountIndex : null;
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
