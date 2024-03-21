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
const WalletRecoveryPhraseStep1Dialog_1 = __importDefault(
  require('../../../../components/wallet/settings/WalletRecoveryPhraseStep1Dialog')
);
const WalletRecoveryPhraseStep2Dialog_1 = __importDefault(
  require('../../../../components/wallet/settings/WalletRecoveryPhraseStep2Dialog')
);
const WalletRecoveryPhraseStep3Dialog_1 = __importDefault(
  require('../../../../components/wallet/settings/WalletRecoveryPhraseStep3Dialog')
);
const WalletRecoveryPhraseStep4Dialog_1 = __importDefault(
  require('../../../../components/wallet/settings/WalletRecoveryPhraseStep4Dialog')
);
const cryptoConfig_1 = require('../../../../config/cryptoConfig');
let WalletRecoveryPhraseContainer = class WalletRecoveryPhraseContainer extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  get step() {
    return this.props.stores.walletSettings.recoveryPhraseStep;
  }
  get recoveryPhraseComponent() {
    const { step } = this;
    if (step === 1) return WalletRecoveryPhraseStep1Dialog_1.default;
    if (step === 2) return WalletRecoveryPhraseStep2Dialog_1.default;
    if (step === 3) return WalletRecoveryPhraseStep3Dialog_1.default;
    if (step === 4) return WalletRecoveryPhraseStep4Dialog_1.default;
    return null;
  }
  render() {
    const { stores, actions } = this.props;
    const { active: activeWallet } = stores.wallets;
    if (!activeWallet) throw new Error('Active wallet required.');
    const {
      recoveryPhraseVerificationContinue,
      recoveryPhraseVerificationCheck,
      recoveryPhraseVerificationClose,
    } = actions.walletSettings;
    const { openExternalLink } = stores.app;
    const onContinue =
      this.step === 2
        ? recoveryPhraseVerificationCheck.trigger
        : recoveryPhraseVerificationContinue.trigger;
    const onClose = recoveryPhraseVerificationClose.trigger;
    const expectedWordCount = activeWallet.isRandom
      ? cryptoConfig_1.LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT
      : cryptoConfig_1.RECOVERY_PHRASE_VERIFICATION_WORD_COUNT;
    const WalletRecoveryPhraseDialog = this.recoveryPhraseComponent;
    return (
      WalletRecoveryPhraseDialog &&
      // @ts-ignore ts-migrate(2604) FIXME: JSX element type 'WalletRecoveryPhraseDialog' does... Remove this comment to see the full error message
      react_1.default.createElement(WalletRecoveryPhraseDialog, {
        onContinue: onContinue,
        onClose: onClose,
        expectedWordCount: expectedWordCount,
        openExternalLink: openExternalLink,
        walletName: activeWallet.name,
      })
    );
  }
};
WalletRecoveryPhraseContainer = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletRecoveryPhraseContainer
);
exports.default = WalletRecoveryPhraseContainer;
//# sourceMappingURL=WalletRecoveryPhraseContainer.js.map
