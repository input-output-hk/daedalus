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
const lodash_1 = require('lodash');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const timingConfig_1 = require('../config/timingConfig');
const getRecoveryWalletIdChannel_1 = require('../ipc/getRecoveryWalletIdChannel');
const walletRecoveryPhraseVerificationUtils_1 = require('../utils/walletRecoveryPhraseVerificationUtils');
const utils_1 = require('../api/utils');
const walletRecoveryPhraseVerificationConfig_1 = require('../config/walletRecoveryPhraseVerificationConfig');
const analytics_1 = require('../analytics');
class WalletSettingsStore extends Store_1.default {
  updateWalletRequest = new LocalizedRequest_1.default(
    this.api.ada.updateWallet
  );
  updateSpendingPasswordRequest = new LocalizedRequest_1.default(
    this.api.ada.updateSpendingPassword
  );
  exportWalletToFileRequest = new LocalizedRequest_1.default(
    this.api.ada.exportWalletToFile
  );
  getWalletUtxosRequest = new LocalizedRequest_1.default(
    this.api.ada.getWalletUtxos
  );
  walletFieldBeingEdited = null;
  lastUpdatedWalletField = null;
  walletUtxos = null;
  recoveryPhraseStep = 0;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  pollingApiInterval = null;
  setup() {
    const {
      walletSettings: walletSettingsActions,
      sidebar: sidebarActions,
    } = this.actions;
    walletSettingsActions.startEditingWalletField.listen(
      this._startEditingWalletField
    );
    walletSettingsActions.stopEditingWalletField.listen(
      this._stopEditingWalletField
    );
    walletSettingsActions.cancelEditingWalletField.listen(
      this._cancelEditingWalletField
    );
    walletSettingsActions.updateWalletField.listen(this._updateWalletField);
    walletSettingsActions.updateSpendingPassword.listen(
      this._updateSpendingPassword
    );
    walletSettingsActions.exportToFile.listen(this._exportToFile);
    walletSettingsActions.startWalletUtxoPolling.listen(
      this._startWalletUtxoPolling
    );
    walletSettingsActions.stopWalletUtxoPolling.listen(
      this._stopWalletUtxoPolling
    );
    walletSettingsActions.recoveryPhraseVerificationContinue.listen(
      this._recoveryPhraseVerificationContinue
    );
    walletSettingsActions.recoveryPhraseVerificationCheck.listen(
      this._recoveryPhraseVerificationCheck
    );
    walletSettingsActions.recoveryPhraseVerificationClose.listen(
      this._recoveryPhraseVerificationClose
    );
    walletSettingsActions.toggleShowUsedAddresses.listen(
      this._toggleShowUsedAddressesStatuses
    );
    sidebarActions.walletSelected.listen(this._onWalletSelected);
  }
  // =================== PUBLIC API ==================== //
  // GETTERS
  getWalletsRecoveryPhraseVerificationData = (walletId) =>
    this.walletsRecoveryPhraseVerificationData[walletId] || {};
  getLocalWalletDataById = (id) => {
    const { all: walletsLocalData } = this.stores.walletsLocal;
    return walletsLocalData[id];
  };
  get walletsRecoveryPhraseVerificationData() {
    const { all: walletsLocalData } = this.stores.walletsLocal;
    return Object.keys(walletsLocalData)
      .map((key) => walletsLocalData[key])
      .reduce((obj, { id, recoveryPhraseVerificationDate, creationDate }) => {
        const {
          recoveryPhraseVerificationStatus,
          recoveryPhraseVerificationStatusType,
        } = (0,
        walletRecoveryPhraseVerificationUtils_1.getStatusFromWalletData)({
          creationDate,
          recoveryPhraseVerificationDate,
        });
        const hasNotification =
          recoveryPhraseVerificationStatus ===
          walletRecoveryPhraseVerificationConfig_1
            .RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION;
        obj[id] = {
          id,
          recoveryPhraseVerificationDate,
          creationDate,
          recoveryPhraseVerificationStatus,
          recoveryPhraseVerificationStatusType,
          hasNotification,
        };
        return obj;
      }, {});
  }
  // =================== PRIVATE API ==================== //
  _startEditingWalletField = ({ field }) => {
    this.walletFieldBeingEdited = field;
  };
  _stopEditingWalletField = () => {
    if (this.walletFieldBeingEdited) {
      this.lastUpdatedWalletField = this.walletFieldBeingEdited;
    }
    this.walletFieldBeingEdited = null;
  };
  _cancelEditingWalletField = () => {
    this.lastUpdatedWalletField = null;
    this.walletFieldBeingEdited = null;
  };
  _updateSpendingPassword = async ({
    walletId,
    oldPassword,
    newPassword,
    isLegacy,
  }) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.updateSpendingPasswordRequest.execute({
      walletId,
      oldPassword,
      newPassword,
      isLegacy,
    });
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.actions.dialogs.closeActiveDialog.trigger();
    this.updateSpendingPasswordRequest.reset();
    this.stores.wallets.refreshWalletsData();
    this.analytics.sendEvent(
      analytics_1.EventCategories.WALLETS,
      'Changed wallet settings',
      'password'
    );
  };
  _updateWalletField = async ({ field, value }) => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;
    const { id: walletId, name, isLegacy } = activeWallet;
    const walletData = {
      walletId,
      name,
      isLegacy,
    };
    walletData[field] = value;
    const wallet = await this.updateWalletRequest.execute({
      walletId: walletData.walletId,
      name: walletData.name,
      isLegacy: walletData.isLegacy,
    }).promise;
    if (!wallet) return;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.stores.wallets.walletsRequest.patch((result) => {
      const walletIndex = (0, lodash_1.findIndex)(result, {
        id: walletId,
      });
      result[walletIndex] = wallet;
    });
    this.updateWalletRequest.reset();
    this.stores.wallets.refreshWalletsData();
    this.analytics.sendEvent(
      analytics_1.EventCategories.WALLETS,
      'Changed wallet settings',
      field
    );
  };
  _exportToFile = async (params) => {
    const { walletId, filePath, password } = params;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.exportWalletToFileRequest.execute({
      walletId,
      filePath,
      password,
    });
    // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
    this.actions.dialogs.closeActiveDialog.trigger();
  };
  _startWalletUtxoPolling = () => {
    this._clearWalletUtxoPollingInterval();
    this._getWalletUtxoApiData();
    this.pollingApiInterval = setInterval(
      this._getWalletUtxoApiData,
      timingConfig_1.WALLET_UTXO_API_REQUEST_INTERVAL
    );
  };
  _stopWalletUtxoPolling = () => {
    this._clearWalletUtxoPollingInterval();
    this.getWalletUtxosRequest.reset();
  };
  _clearWalletUtxoPollingInterval = () => {
    if (this.pollingApiInterval) {
      clearInterval(this.pollingApiInterval);
      this.pollingApiInterval = null;
    }
  };
  _getWalletUtxoApiData = async () => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet) return;
    const { id: walletId, isLegacy } = activeWallet;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    const walletUtxos = await this.getWalletUtxosRequest.execute({
      walletId,
      isLegacy,
    });
    this._updateWalletUtxos(walletUtxos);
  };
  _updateWalletUtxos = (walletUtxos) => {
    this.walletUtxos = walletUtxos;
  };
  _onWalletSelected = () => {
    this._updateWalletUtxos(null);
  };
  /* ==========================================================
    =            Wallet Recovery Phrase Verification            =
    ========================================================== */
  _recoveryPhraseVerificationContinue = async () => {
    const step = this.recoveryPhraseStep;
    if (step === 4) this.recoveryPhraseStep = 2;
    else this.recoveryPhraseStep = step + 1;
  };
  _recoveryPhraseVerificationClose = async () => {
    this.recoveryPhraseStep = 0;
  };
  _recoveryPhraseVerificationCheck = async ({ recoveryPhrase }) => {
    const walletId = await getRecoveryWalletIdChannel_1.getRecoveryWalletIdChannel.request(
      recoveryPhrase
    );
    if (!walletId)
      throw new Error('It was not possible to retrieve the walletId.');
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet)
      throw new Error(
        'Active wallet required before checking recovery phrase.'
      );
    const activeWalletId = (0, utils_1.getRawWalletId)(activeWallet.id);
    const isCorrect = walletId === activeWalletId;
    const nextStep = isCorrect ? 3 : 4;
    this.analytics.sendEvent(
      analytics_1.EventCategories.WALLETS,
      'Verified recovery phrase'
    );
    if (isCorrect) {
      const recoveryPhraseVerificationDate = new Date();
      await this.actions.walletsLocal.setWalletLocalData.trigger({
        walletId: activeWallet.id,
        updatedWalletData: {
          recoveryPhraseVerificationDate,
        },
      });
    }
    (0, mobx_1.runInAction)(
      'AdaWalletBackupStore::_recoveryPhraseVerificationCheck',
      () => {
        this.recoveryPhraseStep = nextStep;
      }
    );
  };
  /* ====  End of Wallet Recovery Phrase Verification  ===== */
  _toggleShowUsedAddressesStatuses = async () => {
    const activeWallet = this.stores.wallets.active;
    if (!activeWallet)
      throw new Error(
        'Active wallet required before checking show used addresses statuses.'
      );
    const localWalletData = this.getLocalWalletDataById(
      activeWallet ? activeWallet.id : ''
    );
    const { showUsedAddresses } = localWalletData || {};
    await this.actions.walletsLocal.setWalletLocalData.trigger({
      walletId: activeWallet.id,
      updatedWalletData: {
        showUsedAddresses: !showUsedAddresses,
      },
    });
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletSettingsStore.prototype,
  'updateWalletRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletSettingsStore.prototype,
  'updateSpendingPasswordRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletSettingsStore.prototype,
  'exportWalletToFileRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  WalletSettingsStore.prototype,
  'getWalletUtxosRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  'walletFieldBeingEdited',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  'lastUpdatedWalletField',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  'walletUtxos',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  'recoveryPhraseStep',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  WalletSettingsStore.prototype,
  'walletsRecoveryPhraseVerificationData',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_startEditingWalletField',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_stopEditingWalletField',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_cancelEditingWalletField',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_updateSpendingPassword',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_updateWalletField',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_exportToFile',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_startWalletUtxoPolling',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_stopWalletUtxoPolling',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_clearWalletUtxoPollingInterval',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_getWalletUtxoApiData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_updateWalletUtxos',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_onWalletSelected',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_recoveryPhraseVerificationContinue',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_recoveryPhraseVerificationClose',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_recoveryPhraseVerificationCheck',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletSettingsStore.prototype,
  '_toggleShowUsedAddressesStatuses',
  void 0
);
exports.default = WalletSettingsStore;
//# sourceMappingURL=WalletSettingsStore.js.map
