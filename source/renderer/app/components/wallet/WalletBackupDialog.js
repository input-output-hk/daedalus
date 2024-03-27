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
const WalletBackupPrivacyWarningDialog_1 = __importDefault(
  require('./backup-recovery/WalletBackupPrivacyWarningDialog')
);
const WalletRecoveryPhraseDisplayDialog_1 = __importDefault(
  require('./backup-recovery/WalletRecoveryPhraseDisplayDialog')
);
const WalletRecoveryPhraseEntryDialog_1 = __importDefault(
  require('./backup-recovery/WalletRecoveryPhraseEntryDialog')
);
const walletBackupTypes_1 = require('../../types/walletBackupTypes');
let WalletBackupDialog = class WalletBackupDialog extends react_1.Component {
  render() {
    const {
      currentStep,
      onCancelBackup,
      canPhraseBeShown,
      isPrivacyNoticeAccepted,
      countdownRemaining,
      onAcceptPrivacyNotice,
      onContinue,
      recoveryPhrase,
      onStartWalletBackup,
      isTermOfflineAccepted,
      enteredPhrase,
      canFinishBackup,
      isTermRecoveryAccepted,
      isValid,
      isSubmitting,
      onAcceptTermOffline,
      onAcceptTermRecovery,
      onUpdateVerificationPhrase,
      onFinishBackup,
      onRestartBackup,
    } = this.props;
    if (
      currentStep === walletBackupTypes_1.WALLET_BACKUP_STEPS.PRIVACY_WARNING
    ) {
      return react_1.default.createElement(
        WalletBackupPrivacyWarningDialog_1.default,
        {
          canPhraseBeShown: canPhraseBeShown,
          isPrivacyNoticeAccepted: isPrivacyNoticeAccepted,
          countdownRemaining: countdownRemaining,
          onAcceptPrivacyNotice: onAcceptPrivacyNotice,
          onCancelBackup: onCancelBackup,
          onContinue: onContinue,
        }
      );
    }
    if (
      currentStep ===
      walletBackupTypes_1.WALLET_BACKUP_STEPS.RECOVERY_PHRASE_DISPLAY
    ) {
      return react_1.default.createElement(
        WalletRecoveryPhraseDisplayDialog_1.default,
        {
          recoveryPhrase: recoveryPhrase,
          onStartWalletBackup: onStartWalletBackup,
          onCancelBackup: onCancelBackup,
          isSubmitting: isSubmitting,
        }
      );
    }
    if (
      currentStep ===
      walletBackupTypes_1.WALLET_BACKUP_STEPS.RECOVERY_PHRASE_ENTRY
    ) {
      return react_1.default.createElement(
        WalletRecoveryPhraseEntryDialog_1.default,
        {
          isTermOfflineAccepted: isTermOfflineAccepted,
          enteredPhrase: enteredPhrase,
          recoveryPhrase: recoveryPhrase,
          canFinishBackup: canFinishBackup,
          isTermRecoveryAccepted: isTermRecoveryAccepted,
          isValid: isValid,
          isSubmitting: isSubmitting,
          onAcceptTermOffline: onAcceptTermOffline,
          onAcceptTermRecovery: onAcceptTermRecovery,
          onUpdateVerificationPhrase: onUpdateVerificationPhrase,
          onCancelBackup: onCancelBackup,
          onFinishBackup: onFinishBackup,
          onRestartBackup: onRestartBackup,
        }
      );
    }
    return null;
  }
};
WalletBackupDialog = __decorate([mobx_react_1.observer], WalletBackupDialog);
exports.default = WalletBackupDialog;
//# sourceMappingURL=WalletBackupDialog.js.map
