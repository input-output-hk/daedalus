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
const mobx_1 = require('mobx');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const asyncForEach_1 = require('../utils/asyncForEach');
class WalletsLocalStore extends Store_1.default {
  localWalletsRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getWalletsLocalData
  );
  setWalletLocalDataRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setWalletLocalData
  );
  unsetWalletLocalDataRequest = new LocalizedRequest_1.default(
    this.api.localStorage.unsetWalletLocalData
  );
  setup() {
    const { walletsLocal: actions } = this.actions;
    actions.refreshWalletsLocalData.listen(this._refreshWalletsLocalData);
    actions.setWalletLocalData.listen(this._setWalletLocalData);
    actions.unsetWalletLocalData.listen(this._unsetWalletLocalData);
    this.localWalletsRequest.execute();
  }
  // =================== PUBLIC API ==================== //
  // GETTERS
  get all() {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'WalletsLocalData | {}' is not assignable to ... Remove this comment to see the full error message
    return this.localWalletsRequest.result
      ? this.localWalletsRequest.result
      : {};
  }
  // =================== PRIVATE API ==================== //
  _refreshWalletsLocalData = async () => {
    const currentLocalWallets = this.all;
    const { all: wallets } = this.stores.wallets;
    await (0, asyncForEach_1.asyncForEach)(
      wallets,
      async ({ id: walletId }) => {
        const walletLocalData = currentLocalWallets[walletId];
        // Adds missing wallets & data
        if (!walletLocalData || !walletLocalData.creationDate) {
          await this._setWalletLocalData({
            walletId,
            skipRefresh: true,
          });
        }
      }
    );
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.localWalletsRequest.execute();
  };
  _setWalletLocalData = async ({
    walletId,
    updatedWalletData,
    skipRefresh,
  }) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setWalletLocalDataRequest.execute(walletId, updatedWalletData);
    if (!skipRefresh) {
      this._refreshWalletsLocalData();
    }
  };
  _unsetWalletLocalData = async ({ walletId }) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.unsetWalletLocalDataRequest.execute(walletId);
    this._refreshWalletsLocalData();
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsLocalStore.prototype,
  'localWalletsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsLocalStore.prototype,
  'setWalletLocalDataRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletsLocalStore.prototype,
  'unsetWalletLocalDataRequest',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  WalletsLocalStore.prototype,
  'all',
  null
);
exports.default = WalletsLocalStore;
//# sourceMappingURL=WalletsLocalStore.js.map
