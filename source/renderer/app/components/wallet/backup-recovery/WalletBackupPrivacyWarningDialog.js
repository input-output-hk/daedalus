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
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const react_intl_1 = require('react-intl');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const WalletRecoveryInstructions_1 = __importDefault(
  require('./WalletRecoveryInstructions')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const cryptoConfig_1 = require('../../../config/cryptoConfig');
const WalletBackupPrivacyWarningDialog_scss_1 = __importDefault(
  require('./WalletBackupPrivacyWarningDialog.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  recoveryPhraseInstructions1: {
    id: 'wallet.backup.privacy.warning.dialog.recoveryPhraseInstructions1',
    defaultMessage:
      '!!!On the following screen, you will be given a list of {walletRecoveryPhraseWordCount}  words to write down on paper and keep in a safe place. This list of words is the wallet recovery phrase for the wallet you are creating.',
    description:
      'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.',
  },
  recoveryPhraseInstructions2: {
    id: 'wallet.backup.privacy.warning.dialog.recoveryPhraseInstructions2',
    defaultMessage:
      '!!!The simplest way to keep your wallet recovery phrase secure is to never store it digitally or online. If you decide to use an online service, such as a password manager with an encrypted database, it is your responsibility to make sure that you use it correctly.',
    description:
      'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.',
  },
  recoveryPhraseInstructions3: {
    id: 'wallet.backup.privacy.warning.dialog.recoveryPhraseInstructions3',
    defaultMessage:
      '!!!<strong>Using your recovery phrase is the only way to recover your wallet if your computer is lost, broken, stolen, or stops working.</strong>',
    description:
      'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.',
  },
  buttonLabelContinue: {
    id: 'wallet.backup.privacy.warning.dialog.button.labelContinue',
    defaultMessage: '!!!Continue',
    description: 'Label for button "Continue" on wallet backup dialog',
  },
  termNobodyWatching: {
    id: 'wallet.backup.privacy.warning.dialog.checkbox.label.nobodyWatching',
    defaultMessage:
      '!!!I confirm that nobody can see my screen, because anyone who knows my recovery phrase will be able to spend the ada in my new wallet.',
    description:
      'Label for the checkbox on wallet backup dialog describing that nobody should be watching when recovery phrase is shown',
  },
});
let WalletBackupPrivacyWarningDialog = class WalletBackupPrivacyWarningDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      countdownRemaining,
      canPhraseBeShown,
      onAcceptPrivacyNotice,
      onCancelBackup,
      isPrivacyNoticeAccepted,
      onContinue,
    } = this.props;
    const countdownDisplay =
      countdownRemaining > 0 ? ` (${countdownRemaining})` : '';
    const dialogClasses = (0, classnames_1.default)([
      WalletBackupPrivacyWarningDialog_scss_1.default.component,
      'WalletBackupPrivacyWarningDialog',
    ]);
    const actions = [
      {
        label:
          intl.formatMessage(messages.buttonLabelContinue) + countdownDisplay,
        onClick: onContinue,
        disabled: !canPhraseBeShown,
        primary: true,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogClasses,
        title: intl.formatMessage(
          global_messages_1.default.recoveryPhraseDialogTitle
        ),
        actions: actions,
        closeOnOverlayClick: false,
        onClose: onCancelBackup,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onCancelBackup }
        ),
      },
      react_1.default.createElement(WalletRecoveryInstructions_1.default, {
        instructionsText: intl.formatMessage(
          messages.recoveryPhraseInstructions1,
          {
            walletRecoveryPhraseWordCount:
              cryptoConfig_1.WALLET_RECOVERY_PHRASE_WORD_COUNT,
          }
        ),
      }),
      react_1.default.createElement(WalletRecoveryInstructions_1.default, {
        instructionsText: intl.formatMessage(
          messages.recoveryPhraseInstructions2
        ),
      }),
      react_1.default.createElement(WalletRecoveryInstructions_1.default, {
        instructionsText: react_1.default.createElement(
          react_intl_1.FormattedHTMLMessage,
          { ...messages.recoveryPhraseInstructions3 }
        ),
      }),
      react_1.default.createElement(
        'div',
        { className: WalletBackupPrivacyWarningDialog_scss_1.default.checkbox },
        react_1.default.createElement(Checkbox_1.Checkbox, {
          label: intl.formatMessage(messages.termNobodyWatching),
          onChange: onAcceptPrivacyNotice,
          checked: isPrivacyNoticeAccepted,
          skin: CheckboxSkin_1.CheckboxSkin,
        })
      )
    );
  }
};
WalletBackupPrivacyWarningDialog = __decorate(
  [mobx_react_1.observer],
  WalletBackupPrivacyWarningDialog
);
exports.default = WalletBackupPrivacyWarningDialog;
//# sourceMappingURL=WalletBackupPrivacyWarningDialog.js.map
