'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const lodash_1 = require('lodash');
const mobx_1 = require('mobx');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedCachedRequest_1 = __importDefault(
  require('./lib/LocalizedCachedRequest')
);
const WalletAddress_1 = __importDefault(require('../domains/WalletAddress'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const LocalizableError_1 = __importDefault(require('../i18n/LocalizableError'));
const crypto_1 = require('../utils/crypto');
class AddressesStore extends Store_1.default {
  lastGeneratedAddress = null;
  addressesRequests = [];
  stakeAddresses = {};
  error = null;
  // REQUESTS
  createByronWalletAddressRequest = new LocalizedRequest_1.default(
    this.api.ada.createAddress
  );
  inspectAddressRequest = new LocalizedRequest_1.default(
    this.api.ada.inspectAddress
  );
  setup() {
    const actions = this.actions.addresses;
    actions.createByronWalletAddress.listen(this._createByronWalletAddress);
    actions.resetErrors.listen(this._resetErrors);
  }
  _createByronWalletAddress = async (params) => {
    try {
      const { walletId, passphrase } = params;
      const accountIndex = await this.getAccountIndexByWalletId(walletId);
      // @ts-ignore ts-migrate(2739) FIXME: Type 'Address' is missing the following properties... Remove this comment to see the full error message
      const address = await this.createByronWalletAddressRequest.execute({
        addressIndex: accountIndex,
        passphrase,
        walletId,
      }).promise;
      if (address != null) {
        this._refreshAddresses();
        (0, mobx_1.runInAction)(
          'set last generated address and reset error',
          () => {
            this.lastGeneratedAddress = address;
            this.error = null;
          }
        );
      }
    } catch (error) {
      (0, mobx_1.runInAction)('set error', () => {
        this.error = error;
      });
    }
  };
  _inspectAddress = async (params) => {
    const { addressId } = params;
    this.inspectAddressRequest.reset();
    const addressDetails = await this.inspectAddressRequest.execute({
      addressId,
    }).promise;
    return addressDetails;
  };
  get all() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return [];
    const addresses = this._getAddressesAllRequest(wallet.id).result;
    return addresses || [];
  }
  get hasAny() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return false;
    const addresses = this._getAddressesAllRequest(wallet.id).result;
    return addresses ? addresses.length > 0 : false;
  }
  get active() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return null;
    // If address generated and not used, set as active address
    if (this.lastGeneratedAddress && !this.lastGeneratedAddress.used)
      return this.lastGeneratedAddress;
    // Check if wallet has addresses
    const addresses = this._getAddressesAllRequest(wallet.id).result;
    if (!addresses) return null;
    // Check if there is any unused address and set last as active
    const unusedAddresses = (0, lodash_1.filter)(
      addresses,
      (address) => !address.used
    );
    if (unusedAddresses.length) return (0, lodash_1.last)(unusedAddresses);
    // Set last used address as active
    return (0, lodash_1.last)(addresses);
  }
  get totalAvailable() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return 0;
    const addresses = this._getAddressesAllRequest(wallet.id).result;
    return addresses ? addresses.length : 0;
  }
  get stakeAddress() {
    const wallet = this.stores.wallets.active;
    if (!wallet) return '';
    return this.stakeAddresses[wallet.id] || '';
  }
  _getStakeAddress = async (walletId, isLegacy) => {
    const hasStakeAddress = (0, lodash_1.has)(this.stakeAddresses, walletId);
    if (!hasStakeAddress) {
      if (isLegacy) {
        this.stakeAddresses[walletId] = '';
      } else {
        const getWalletStakeKeyRequest = new LocalizedRequest_1.default(
          this.api.ada.getWalletPublicKey
        );
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        const stakeKeyBech32 = await getWalletStakeKeyRequest.execute({
          walletId,
          role: 'mutable_account',
          index: '0',
        });
        const stakeAddress = (0, crypto_1.getStakeAddressFromStakeKey)(
          stakeKeyBech32
        );
        (0, mobx_1.runInAction)('set stake address', () => {
          this.stakeAddresses[walletId] = stakeAddress;
        });
      }
    }
  };
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
  _resetErrors = () => {
    this.error = null;
  };
  isInternalAddress = (address) => {
    return (
      (0, lodash_1.findIndex)(this.all, {
        id: address,
      }) > -1
    );
  };
  getAddressIndex = (address) => {
    return (
      this.all.length -
      (0, lodash_1.findIndex)(this.all, {
        id: address,
      }) -
      1
    );
  };
  getAccountIndexByWalletId = async (walletId) => {
    // @ts-ignore
    const result = await this.api.ada.getAddresses({
      walletId,
      isLegacy: true,
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'accountIndex' does not exist on type 'Wa... Remove this comment to see the full error message
    return result ? result.accountIndex : null;
  };
  getAddressesByWalletId = async (walletId) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    const addresses = await this._getAddressesAllRequest(walletId);
    return addresses || [];
  };
  _getAddressesAllRequest = (walletId) => {
    const foundRequest = (0, lodash_1.find)(this.addressesRequests, {
      walletId,
    });
    if (foundRequest && foundRequest.allRequest) return foundRequest.allRequest;
    return new LocalizedCachedRequest_1.default(this.api.ada.getAddresses);
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', WalletAddress_1.default)],
  AddressesStore.prototype,
  'lastGeneratedAddress',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  AddressesStore.prototype,
  'addressesRequests',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AddressesStore.prototype,
  'stakeAddresses',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizableError_1.default)],
  AddressesStore.prototype,
  'error',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  AddressesStore.prototype,
  'createByronWalletAddressRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  AddressesStore.prototype,
  'inspectAddressRequest',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  AddressesStore.prototype,
  'all',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  AddressesStore.prototype,
  'hasAny',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', WalletAddress_1.default),
    __metadata('design:paramtypes', []),
  ],
  AddressesStore.prototype,
  'active',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  AddressesStore.prototype,
  'totalAvailable',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  AddressesStore.prototype,
  'stakeAddress',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AddressesStore.prototype,
  '_getStakeAddress',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AddressesStore.prototype,
  '_refreshAddresses',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AddressesStore.prototype,
  '_resetErrors',
  void 0
);
exports.default = AddressesStore;
//# sourceMappingURL=AddressesStore.js.map
