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
exports.WalletUnits = exports.HwDeviceStatuses = exports.WalletDelegationStatuses = exports.WalletSyncStateStatuses = exports.WalletDiscovery = void 0;
const lodash_1 = require('lodash');
const mobx_1 = require('mobx');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
exports.WalletDiscovery = {
  RANDOM: 'random',
  SEQUENTIAL: 'sequential',
};
exports.WalletSyncStateStatuses = {
  RESTORING: 'syncing',
  // @API TODO - calculate if the wallet is restoring!
  SYNCING: 'syncing',
  READY: 'ready',
  NOT_RESPONDING: 'not_responding',
};
exports.WalletDelegationStatuses = {
  DELEGATING: 'delegating',
  NOT_DELEGATING: 'not_delegating',
};
exports.HwDeviceStatuses = {
  CONNECTING: 'connecting',
  CONNECTING_FAILED: 'connecting_failed',
  TREZOR_BRIDGE_FAILURE: 'trezor_bridge_failure',
  LAUNCHING_CARDANO_APP: 'launching_cardano_app',
  EXPORTING_PUBLIC_KEY: 'exporting_public_key',
  EXPORTING_PUBLIC_KEY_FAILED: 'exporting_public_key_failed',
  WRONG_FIRMWARE: 'wrong_firmware',
  WRONG_CARDANO_APP_VERSION: 'wrong_cardano_app_version',
  UNSUPPORTED_DEVICE: 'unsupported_device',
  READY: 'ready',
  VERIFYING_TRANSACTION: 'verifying_transaction',
  VERIFYING_TRANSACTION_FAILED: 'verifying_transaction_failed',
  VERIFYING_TRANSACTION_SUCCEEDED: 'verifying_transaction_succeeded',
  VERIFYING_ADDRESS: 'verifying_address',
  VERIFYING_ADDRESS_CONFIRMATION: 'verifying_address_confirmation',
  VERIFYING_ADDRESS_FAILED: 'verifying_address_failed',
  VERIFYING_ADDRESS_ABORTED: 'verifying_address_aborted',
  VERIFYING_ADDRESS_SUCCEEDED: 'verifying_address_succeeded',
  UNRECOGNIZED_WALLET: 'unrecognized_wallet',
};
var WalletUnits;
(function (WalletUnits) {
  WalletUnits['ADA'] = 'ada';
  WalletUnits['LOVELACE'] = 'lovelace';
})((WalletUnits = exports.WalletUnits || (exports.WalletUnits = {})));
class Wallet {
  id = '';
  addressPoolGap;
  name = '';
  amount;
  availableAmount;
  reward;
  assets;
  passwordUpdateDate;
  syncState;
  isLegacy;
  delegatedStakePoolId;
  delegationStakePoolStatus;
  lastDelegatedStakePoolId;
  lastDelegationStakePoolStatus;
  pendingDelegations;
  discovery;
  hasPassword;
  walletNotConnected;
  isHardwareWallet;
  constructor(data) {
    Object.assign(this, data);
  }
  update(other) {
    Object.assign(
      this,
      (0, lodash_1.pick)(other, [
        'id',
        'addressPoolGap',
        'name',
        'amount',
        'availableAmount',
        'reward',
        'assets',
        'passwordUpdateDate',
        'syncState',
        'isLegacy',
        'delegatedStakePoolId',
        'delegationStakePoolStatus',
        'lastDelegatedStakePoolId',
        'lastDelegationStakePoolStatus',
        'pendingDelegations',
        'discovery',
        'hasPassword',
        'walletNotConnected',
        'isHardwareWallet',
      ])
    );
  }
  get hasFunds() {
    return this.amount.gt(0);
  }
  get hasAssets() {
    return (0, lodash_1.get)(this, 'assets.total', []).length > 0;
  }
  get isRestoring() {
    return (
      (0, lodash_1.get)(this, 'syncState.status') ===
        exports.WalletSyncStateStatuses.RESTORING &&
      this.restorationProgress < 100
    );
  }
  get isSyncing() {
    return (
      (0, lodash_1.get)(this, 'syncState.status') ===
      exports.WalletSyncStateStatuses.SYNCING
    );
  }
  get isNotResponding() {
    return (
      (0, lodash_1.get)(this, 'syncState.status') ===
      exports.WalletSyncStateStatuses.NOT_RESPONDING
    );
  }
  get isRandom() {
    return this.discovery === exports.WalletDiscovery.RANDOM;
  }
  get isDelegating() {
    return this.lastDelegationStakePoolStatus
      ? this.lastDelegationStakePoolStatus ===
          exports.WalletDelegationStatuses.DELEGATING
      : this.delegationStakePoolStatus ===
          exports.WalletDelegationStatuses.DELEGATING;
  }
  get isSequential() {
    return this.discovery !== exports.WalletDiscovery.RANDOM;
  }
  get restorationProgress() {
    return (0, lodash_1.get)(this, 'syncState.progress.quantity', 0);
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  Wallet.prototype,
  'addressPoolGap',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Wallet.prototype,
  'name',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  Wallet.prototype,
  'amount',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  Wallet.prototype,
  'availableAmount',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  Wallet.prototype,
  'reward',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Wallet.prototype,
  'assets',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Date)],
  Wallet.prototype,
  'passwordUpdateDate',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Wallet.prototype,
  'syncState',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  Wallet.prototype,
  'isLegacy',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  Wallet.prototype,
  'delegatedStakePoolId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  Wallet.prototype,
  'delegationStakePoolStatus',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  Wallet.prototype,
  'lastDelegatedStakePoolId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  Wallet.prototype,
  'lastDelegationStakePoolStatus',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  Wallet.prototype,
  'pendingDelegations',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  Wallet.prototype,
  'discovery',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  Wallet.prototype,
  'hasPassword',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  Wallet.prototype,
  'walletNotConnected',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  Wallet.prototype,
  'isHardwareWallet',
  void 0
);
__decorate(
  [
    mobx_1.action,
    __metadata('design:type', Function),
    __metadata('design:paramtypes', [Wallet]),
    __metadata('design:returntype', void 0),
  ],
  Wallet.prototype,
  'update',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  Wallet.prototype,
  'hasFunds',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  Wallet.prototype,
  'hasAssets',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  Wallet.prototype,
  'isRestoring',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  Wallet.prototype,
  'isSyncing',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  Wallet.prototype,
  'isNotResponding',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  Wallet.prototype,
  'isRandom',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  Wallet.prototype,
  'isDelegating',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  Wallet.prototype,
  'isSequential',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  Wallet.prototype,
  'restorationProgress',
  null
);
exports.default = Wallet;
//# sourceMappingURL=Wallet.js.map
