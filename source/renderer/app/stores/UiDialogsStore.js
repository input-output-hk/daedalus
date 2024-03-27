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
const WalletReceiveDialog_1 = __importDefault(
  require('../components/wallet/receive/WalletReceiveDialog')
);
const AssetSettingsDialog_1 = __importDefault(
  require('../components/assets/AssetSettingsDialog')
);
const DelegationSetupWizardDialog_1 = __importDefault(
  require('../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog')
);
const analytics_1 = require('../analytics');
class UiDialogsStore extends Store_1.default {
  activeDialog = null;
  secondsSinceActiveDialogIsOpen = 0;
  dataForActiveDialog = {};
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  _secondsTimerInterval = null;
  setup() {
    this.actions.dialogs.open.listen(this._onOpen);
    this.actions.dialogs.closeActiveDialog.listen(this._onClose);
    this.actions.dialogs.resetActiveDialog.listen(this._reset);
    this.actions.dialogs.updateDataForActiveDialog.listen(
      this._onUpdateDataForActiveDialog
    );
  }
  isOpen = (dialog) => this.activeDialog === dialog;
  countdownSinceDialogOpened = (countDownTo) =>
    Math.max(countDownTo - this.secondsSinceActiveDialogIsOpen, 0);
  _onOpen = ({ dialog }) => {
    this._reset();
    this.activeDialog = dialog;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'defaultProps' does not exist on type '(.... Remove this comment to see the full error message
    this.dataForActiveDialog = (0, mobx_1.observable)(
      dialog.defaultProps || {}
    );
    this.secondsSinceActiveDialogIsOpen = 0;
    if (this._secondsTimerInterval) clearInterval(this._secondsTimerInterval);
    this._secondsTimerInterval = setInterval(this._updateSeconds, 1000);
    this._handleAnalytics(dialog);
  };
  _onClose = () => {
    this._reset();
  };
  _updateSeconds = () => {
    this.secondsSinceActiveDialogIsOpen += 1;
  };
  _onUpdateDataForActiveDialog = ({ data }) => {
    Object.assign(this.dataForActiveDialog, data);
  };
  _reset = () => {
    this.activeDialog = null;
    this.secondsSinceActiveDialogIsOpen = 0;
    this.dataForActiveDialog = {};
  };
  _handleAnalytics = (dialog) => {
    switch (dialog) {
      case WalletReceiveDialog_1.default:
        this.analytics.sendEvent(
          analytics_1.EventCategories.WALLETS,
          'Opened share wallet address modal'
        );
        break;
      case AssetSettingsDialog_1.default:
        this.analytics.sendEvent(
          analytics_1.EventCategories.WALLETS,
          'Opened native token settings'
        );
        break;
      case DelegationSetupWizardDialog_1.default:
        this.analytics.sendEvent(
          analytics_1.EventCategories.STAKE_POOLS,
          'Opened delegate wallet dialog'
        );
        break;
      default:
        break;
    }
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  UiDialogsStore.prototype,
  'activeDialog',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  UiDialogsStore.prototype,
  'secondsSinceActiveDialogIsOpen',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  UiDialogsStore.prototype,
  'dataForActiveDialog',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  UiDialogsStore.prototype,
  '_onOpen',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  UiDialogsStore.prototype,
  '_onClose',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  UiDialogsStore.prototype,
  '_updateSeconds',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  UiDialogsStore.prototype,
  '_onUpdateDataForActiveDialog',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  UiDialogsStore.prototype,
  '_reset',
  void 0
);
exports.default = UiDialogsStore;
//# sourceMappingURL=UiDialogsStore.js.map
