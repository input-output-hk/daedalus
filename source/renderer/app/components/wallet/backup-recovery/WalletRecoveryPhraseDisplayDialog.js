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
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const WalletRecoveryInstructions_1 = __importDefault(
  require('./WalletRecoveryInstructions')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const WalletRecoveryPhraseDisplayDialog_scss_1 = __importDefault(
  require('./WalletRecoveryPhraseDisplayDialog.scss')
);
const cryptoConfig_1 = require('../../../config/cryptoConfig');
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const mnemonic_input_1 = require('../mnemonic-input');
const messages = (0, react_intl_1.defineMessages)({
  backupInstructions: {
    id: 'wallet.backup.recovery.phrase.display.dialog.backup.instructions',
    defaultMessage:
      '!!!Please make sure you write down the {walletRecoveryPhraseWordCount} words of your wallet recovery phrase <strong>on a piece of paper in the exact order shown here</strong>.',
    description:
      'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.',
  },
  buttonLabelIHaveWrittenItDown: {
    id:
      'wallet.backup.recovery.phrase.display.dialog.button.label.iHaveWrittenItDown',
    defaultMessage: '!!!Yes, I have written down my wallet recovery phrase.',
    description:
      'Label for button "Yes, I have written down my wallet recovery phrase." on wallet backup dialog',
  },
});
let WalletRecoveryPhraseDisplayDialog = class WalletRecoveryPhraseDisplayDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      recoveryPhrase,
      onStartWalletBackup,
      onCancelBackup,
      isSubmitting,
    } = this.props;
    const dialogClasses = (0, classnames_1.default)([
      WalletRecoveryPhraseDisplayDialog_scss_1.default.component,
      'WalletRecoveryPhraseDisplayDialog',
    ]);
    const buttonLabel = !isSubmitting
      ? intl.formatMessage(messages.buttonLabelIHaveWrittenItDown)
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const actions = [
      {
        label: buttonLabel,
        onClick: onStartWalletBackup,
        primary: true,
      },
    ];
    const mnemonicValues = recoveryPhrase.split(' ');
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogClasses,
        title: intl.formatMessage(
          global_messages_1.default.recoveryPhraseDialogTitle
        ),
        actions: actions,
        onClose: onCancelBackup,
        closeOnOverlayClick: false,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onCancelBackup }
        ),
      },
      react_1.default.createElement(WalletRecoveryInstructions_1.default, {
        instructionsText: react_1.default.createElement(
          react_intl_1.FormattedHTMLMessage,
          {
            ...messages.backupInstructions,
            values: {
              walletRecoveryPhraseWordCount:
                cryptoConfig_1.WALLET_RECOVERY_PHRASE_WORD_COUNT,
            },
          }
        ),
      }),
      react_1.default.createElement(mnemonic_input_1.MnemonicInput, {
        disabled: true,
        value: mnemonicValues,
        wordCount: mnemonicValues.length,
      })
    );
  }
};
WalletRecoveryPhraseDisplayDialog = __decorate(
  [mobx_react_1.observer],
  WalletRecoveryPhraseDisplayDialog
);
exports.default = WalletRecoveryPhraseDisplayDialog;
//# sourceMappingURL=WalletRecoveryPhraseDisplayDialog.js.map
