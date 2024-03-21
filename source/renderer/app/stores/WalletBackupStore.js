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
const WalletBackupDialog_1 = __importDefault(
  require('../components/wallet/WalletBackupDialog')
);
const walletBackupTypes_1 = require('../types/walletBackupTypes');
class WalletBackupStore extends Store_1.default {
  inProgress = false;
  currentStep = walletBackupTypes_1.WALLET_BACKUP_STEPS.NOT_INITIATED;
  recoveryPhrase = [];
  completed = false;
  enteredPhrase = [];
  isPrivacyNoticeAccepted = false;
  isEntering = false;
  isTermOfflineAccepted = false;
  isTermRecoveryAccepted = false;
  countdownRemaining = 0;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  countdownTimerInterval = null;
  setup() {
    const a = this.actions.walletBackup;
    a.initiateWalletBackup.listen(this._initiateWalletBackup);
    a.acceptPrivacyNoticeForWalletBackup.listen(
      this._acceptPrivacyNoticeForWalletBackup
    );
    a.continueToRecoveryPhraseForWalletBackup.listen(
      this._continueToRecoveryPhraseForWalletBackup
    );
    a.startWalletBackup.listen(this._startWalletBackup);
    a.updateWalletBackupVerificationPhrase.listen(
      this._updateWalletBackupVerificationPhrase
    );
    a.acceptWalletBackupTermOffline.listen(this._acceptWalletBackupTermOffline);
    a.acceptWalletBackupTermRecovery.listen(
      this._acceptWalletBackupTermRecovery
    );
    a.restartWalletBackup.listen(this._restartWalletBackup);
    a.cancelWalletBackup.listen(this._cancelWalletBackup);
    a.finishWalletBackup.listen(this._finishWalletBackup);
    this.actions.app.initAppEnvironment.listen(() => {});
  }
  _initiateWalletBackup = (params) => {
    this.recoveryPhrase = params.recoveryPhrase;
    this.inProgress = true;
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'walletBac... Remove this comment to see the full error message
    this.currentStep = walletBackupTypes_1.WALLET_BACKUP_STEPS.PRIVACY_WARNING;
    this.completed = false;
    this.enteredPhrase = [];
    this.isPrivacyNoticeAccepted = false;
    this.isEntering = false;
    this.isTermOfflineAccepted = false;
    this.isTermRecoveryAccepted = false;
    this.countdownRemaining = this.environment.isTest ? 0 : 10;
    if (this.countdownTimerInterval) clearInterval(this.countdownTimerInterval);
    this.countdownTimerInterval = setInterval(() => {
      if (this.countdownRemaining > 0) {
        (0, mobx_1.action)(() => this.countdownRemaining--)();
      } else if (this.countdownTimerInterval != null) {
        clearInterval(this.countdownTimerInterval);
      }
    }, 1000);
    this.actions.dialogs.open.trigger({
      // @ts-ignore ts-migrate(2322) FIXME: Type 'typeof WalletBackupDialog' is not assignable... Remove this comment to see the full error message
      dialog: WalletBackupDialog_1.default,
    });
  };
  _acceptPrivacyNoticeForWalletBackup = () => {
    this.isPrivacyNoticeAccepted = true;
  };
  _continueToRecoveryPhraseForWalletBackup = () => {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'walletBac... Remove this comment to see the full error message
    this.currentStep =
      walletBackupTypes_1.WALLET_BACKUP_STEPS.RECOVERY_PHRASE_DISPLAY;
  };
  _startWalletBackup = () => {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'walletBac... Remove this comment to see the full error message
    this.currentStep =
      walletBackupTypes_1.WALLET_BACKUP_STEPS.RECOVERY_PHRASE_ENTRY;
  };
  _updateWalletBackupVerificationPhrase = (params) => {
    const { verificationPhrase } = params;
    this.enteredPhrase = verificationPhrase;
  };
  _clearEnteredRecoveryPhrase = () => {
    this.enteredPhrase = [];
  };
  get isRecoveryPhraseValid() {
    return this.recoveryPhrase.join(' ') === this.enteredPhrase.join(' ');
  }
  _acceptWalletBackupTermOffline = () => {
    this.isTermOfflineAccepted = true;
  };
  _acceptWalletBackupTermRecovery = () => {
    this.isTermRecoveryAccepted = true;
  };
  _restartWalletBackup = () => {
    this._clearEnteredRecoveryPhrase();
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'walletBac... Remove this comment to see the full error message
    this.currentStep =
      walletBackupTypes_1.WALLET_BACKUP_STEPS.RECOVERY_PHRASE_DISPLAY;
  };
  _cancelWalletBackup = () => {
    this.inProgress = false;
    this._clearEnteredRecoveryPhrase();
  };
  _finishWalletBackup = () => {
    this.inProgress = false;
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  'inProgress',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletBackupStore.prototype,
  'currentStep',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  'recoveryPhrase',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  'completed',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  'enteredPhrase',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  'isPrivacyNoticeAccepted',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  'isEntering',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  'isTermOfflineAccepted',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  'isTermRecoveryAccepted',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  'countdownRemaining',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_initiateWalletBackup',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_acceptPrivacyNoticeForWalletBackup',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_continueToRecoveryPhraseForWalletBackup',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_startWalletBackup',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_updateWalletBackupVerificationPhrase',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_clearEnteredRecoveryPhrase',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  WalletBackupStore.prototype,
  'isRecoveryPhraseValid',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_acceptWalletBackupTermOffline',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_acceptWalletBackupTermRecovery',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_restartWalletBackup',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_cancelWalletBackup',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  WalletBackupStore.prototype,
  '_finishWalletBackup',
  void 0
);
exports.default = WalletBackupStore;
//# sourceMappingURL=WalletBackupStore.js.map
