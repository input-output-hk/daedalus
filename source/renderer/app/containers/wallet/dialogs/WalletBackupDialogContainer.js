'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
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
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const WalletBackupDialog_1 = __importDefault(
  require('../../../components/wallet/WalletBackupDialog')
);
let WalletBackupDialogContainer = class WalletBackupDialogContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  onCancelBackup = () => {
    this.props.onClose();
    this.props.actions.walletBackup.cancelWalletBackup.trigger();
  };
  render() {
    const { actions, stores } = this.props;
    const {
      enteredPhrase,
      isRecoveryPhraseValid,
      countdownRemaining,
      isTermOfflineAccepted,
      isTermRecoveryAccepted,
      isPrivacyNoticeAccepted,
      currentStep,
      recoveryPhrase,
    } = stores.walletBackup;
    const {
      startWalletBackup,
      updateWalletBackupVerificationPhrase,
      acceptWalletBackupTermOffline,
      acceptWalletBackupTermRecovery,
      restartWalletBackup,
      finishWalletBackup,
      acceptPrivacyNoticeForWalletBackup,
      continueToRecoveryPhraseForWalletBackup,
    } = actions.walletBackup;
    const { createWalletRequest } = stores.wallets;
    const canFinishBackup =
      isRecoveryPhraseValid && isTermOfflineAccepted && isTermRecoveryAccepted;
    return react_1.default.createElement(
      WalletBackupDialog_1.default, // Global props for all dialogs
      {
        currentStep: currentStep,
        onCancelBackup: this.onCancelBackup,
        canPhraseBeShown: isPrivacyNoticeAccepted && countdownRemaining === 0,
        isPrivacyNoticeAccepted: isPrivacyNoticeAccepted,
        countdownRemaining: countdownRemaining,
        onAcceptPrivacyNotice: acceptPrivacyNoticeForWalletBackup.trigger,
        onContinue: continueToRecoveryPhraseForWalletBackup.trigger,
        recoveryPhrase: recoveryPhrase.join(' '),
        onStartWalletBackup: startWalletBackup.trigger,
        isTermOfflineAccepted: isTermOfflineAccepted,
        enteredPhrase: enteredPhrase,
        canFinishBackup: canFinishBackup,
        isTermRecoveryAccepted: isTermRecoveryAccepted,
        isValid: isRecoveryPhraseValid,
        isSubmitting: createWalletRequest.isExecuting,
        onAcceptTermOffline: acceptWalletBackupTermOffline.trigger,
        onAcceptTermRecovery: acceptWalletBackupTermRecovery.trigger,
        onUpdateVerificationPhrase:
          updateWalletBackupVerificationPhrase.trigger,
        onFinishBackup: () => {
          finishWalletBackup.trigger();
        },
        onRestartBackup: restartWalletBackup.trigger,
      }
    );
  }
};
WalletBackupDialogContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletBackupDialogContainer
);
exports.default = WalletBackupDialogContainer;
//# sourceMappingURL=WalletBackupDialogContainer.js.map
