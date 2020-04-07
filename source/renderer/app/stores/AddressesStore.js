// @flow
import { find, last, filter } from 'lodash';
import { observable, computed, action, runInAction } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/LocalizedCachedRequest';
import WalletAddress from '../domains/WalletAddress';
import Request from './lib/LocalizedRequest';
import LocalizableError from '../i18n/LocalizableError';
import type { Address } from '../api/addresses/types';

export default class AddressesStore extends Store {
  @observable lastGeneratedAddress: ?WalletAddress = null;
  @observable addressesRequests: Array<{
    walletId: string,
    isLegacy: boolean,
    allRequest: CachedRequest<Array<WalletAddress>>,
  }> = [];
  @observable error: ?LocalizableError = null;

  // REQUESTS
  @observable createByronWalletAddressRequest: Request<Address> = new Request(
    this.api.ada.createAddress
  );

  setup() {
    const actions = this.actions.addresses;
    actions.createByronWalletAddress.listen(this._createByronWalletAddress);
    actions.resetErrors.listen(this._resetErrors);
  }

  _createByronWalletAddress = async (params: {
    walletId: string,
    passphrase: string,
  }) => {
    try {
      const { walletId, passphrase } = params;
      const accountIndex = await this.getAccountIndexByWalletId(walletId);

      const address: ?Address = await this.createByronWalletAddressRequest.execute(
        {
          addressIndex: accountIndex,
          passphrase,
          walletId,
        }
      ).promise;

      if (address != null) {
        this._refreshAddresses();
        runInAction('set last generated address and reset error', () => {
          this.lastGeneratedAddress = new WalletAddress({
            id: address.id,
            used: address.state === 'used',
          });
          this.error = null;
        });
      }
    } catch (error) {
      runInAction('set error', () => {
        this.error = error;
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

    // If address generated and not used, set as active address
    if (this.lastGeneratedAddress && !this.lastGeneratedAddress.used)
      return this.lastGeneratedAddress;

    // Check if wallet has addresses
    const addresses = this._getAddressesAllRequest(wallet.id).result;
    if (!addresses) return null;

    // Check if there is any unused address and se last as active
    const unusedAddresses = filter(addresses, address => !address.used);
    if (unusedAddresses.length) return last(unusedAddresses);

    // Set last used address as active
    return last(addresses);
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
        const { id: walletId, isLegacy } = wallet;
        const allRequest = this._getAddressesAllRequest(walletId);
        allRequest.invalidate({ immediately: false });
        allRequest.execute({ walletId, isLegacy });
      }
    }
  };

  @action _resetErrors = () => {
    this.error = null;
  };

  getAccountIndexByWalletId = async (walletId: string): Promise<?number> => {
    // $FlowFixMe
    const result = await this.api.ada.getAddresses({
      walletId,
      isLegacy: true,
    });
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
