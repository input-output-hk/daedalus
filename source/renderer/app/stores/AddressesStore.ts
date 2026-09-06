import { has, find, last, filter, findIndex } from 'lodash';
import { observable, computed, action, runInAction } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/LocalizedCachedRequest';
import WalletAddress from '../domains/WalletAddress';
import Request from './lib/LocalizedRequest';
import LocalizableError from '../i18n/LocalizableError';
import { getStakeAddressFromStakeKey } from '../utils/crypto';
import type { Address, InspectAddressResponse } from '../api/addresses/types';

export default class AddressesStore extends Store {
  @observable
  lastGeneratedAddress: WalletAddress | null | undefined = null;
  @observable
  addressesRequests: Array<{
    walletId: string;
    isLegacy: boolean;
    allRequest: CachedRequest<Array<WalletAddress>>;
  }> = [];
  @observable
  stakeAddresses: Record<string, string> = {};
  @observable
  error: LocalizableError | null | undefined = null;
  // REQUESTS
  @observable
  createByronWalletAddressRequest: Request<Address> = new Request(
    this.api.ada.createAddress
  );
  @observable
  inspectAddressRequest: Request<InspectAddressResponse> = new Request(
    this.api.ada.inspectAddress
  );

  setup() {
    const actions = this.actions.addresses;
    actions.createByronWalletAddress.listen(this._createByronWalletAddress);
    actions.resetErrors.listen(this._resetErrors);
  }

  _createByronWalletAddress = async (params: {
    walletId: string;
    passphrase: string;
  }) => {
    try {
      const { walletId, passphrase } = params;
      const accountIndex = await this.getAccountIndexByWalletId(walletId);
      // @ts-ignore ts-migrate(2739) FIXME: Type 'Address' is missing the following properties... Remove this comment to see the full error message
      const address: WalletAddress = await this.createByronWalletAddressRequest.execute(
        {
          addressIndex: accountIndex,
          passphrase,
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
        this.error = error;
      });
    }
  };
  _inspectAddress = async (params: { addressId: string }) => {
    const { addressId } = params;
    this.inspectAddressRequest.reset();
    const addressDetails = await this.inspectAddressRequest.execute({
      addressId,
    }).promise;
    return addressDetails;
  };

  @computed
  get all(): Array<WalletAddress> {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];

    const addresses = this._getAddressesAllRequest(wallet.id).result;

    return addresses || [];
  }

  @computed
  get hasAny(): boolean {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;

    const addresses = this._getAddressesAllRequest(wallet.id).result;

    return addresses ? addresses.length > 0 : false;
  }

  @computed
  get active(): WalletAddress | null | undefined {
    const wallet = this.stores.wallets.active;
    if (!wallet) return null;
    // If address generated and not used, set as active address
    if (this.lastGeneratedAddress && !this.lastGeneratedAddress.used)
      return this.lastGeneratedAddress;

    // Check if wallet has addresses
    const addresses = this._getAddressesAllRequest(wallet.id).result;

    if (!addresses) return null;
    // Check if there is any unused address and set last as active
    const unusedAddresses = filter(addresses, (address) => !address.used);
    if (unusedAddresses.length) return last(unusedAddresses);
    // Set last used address as active
    return last(addresses);
  }

  @computed
  get totalAvailable(): number {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;

    const addresses = this._getAddressesAllRequest(wallet.id).result;

    return addresses ? addresses.length : 0;
  }

  @computed
  get stakeAddress(): string {
    const wallet = this.stores.wallets.active;
    if (!wallet) return '';
    return this.stakeAddresses[wallet.id] || '';
  }

  @action
  _getStakeAddress = async (walletId: string, isLegacy: boolean) => {
    const hasStakeAddress = has(this.stakeAddresses, walletId);

    if (!hasStakeAddress) {
      if (isLegacy) {
        this.stakeAddresses[walletId] = '';
      } else {
        const getWalletStakeKeyRequest = new Request(
          this.api.ada.getWalletPublicKey
        );
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        const stakeKeyBech32 = await getWalletStakeKeyRequest.execute({
          walletId,
          role: 'mutable_account',
          index: '0',
        });
        const stakeAddress = getStakeAddressFromStakeKey(stakeKeyBech32);
        runInAction('set stake address', () => {
          this.stakeAddresses[walletId] = stakeAddress;
        });
      }
    }
  };
  @action
  _refreshAddresses = () => {
    if (this.stores.networkStatus.isConnected) {
      const { all } = this.stores.wallets;

      for (const wallet of all) {
        const { id: walletId, isLegacy } = wallet;

        const allRequest = this._getAddressesAllRequest(walletId);

        allRequest.invalidate({
          immediately: false,
        });
        allRequest.execute({
          walletId,
          isLegacy,
        });

        this._getStakeAddress(walletId, isLegacy);
      }
    }
  };
  @action
  _resetErrors = () => {
    this.error = null;
  };
  isInternalAddress = (address: string): boolean => {
    return (
      findIndex(this.all, {
        id: address,
      }) > -1
    );
  };
  getAddressIndex = (address: string): number => {
    return (
      this.all.length -
      findIndex(this.all, {
        id: address,
      }) -
      1
    );
  };
  getAccountIndexByWalletId = async (
    walletId: string
  ): Promise<number | null | undefined> => {
    // @ts-ignore
    const result = await this.api.ada.getAddresses({
      walletId,
      isLegacy: true,
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'accountIndex' does not exist on type 'Wa... Remove this comment to see the full error message
    return result ? result.accountIndex : null;
  };
  getAddressesByWalletId = async (
    walletId: string
  ): Promise<Array<WalletAddress>> => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    const addresses = await this._getAddressesAllRequest(walletId);
    return addresses || [];
  };
  _getAddressesAllRequest = (
    walletId: string
  ): CachedRequest<Array<WalletAddress>> => {
    const foundRequest = find(this.addressesRequests, {
      walletId,
    });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new CachedRequest(this.api.ada.getAddresses);
  };
}
