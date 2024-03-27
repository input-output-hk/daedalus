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
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const react_intl_1 = require('react-intl');
const cryptoConfig_1 = require('../../../config/cryptoConfig');
const valid_words_en_1 = __importDefault(
  require('../../../../../common/config/crypto/valid-words.en')
);
const decrypt_1 = require('../../../../../common/config/crypto/decrypt');
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const validations_1 = require('../../../utils/validations');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const DialogBackButton_1 = __importDefault(
  require('../../widgets/DialogBackButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const WalletRecoveryInstructions_1 = __importDefault(
  require('./WalletRecoveryInstructions')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const WalletRecoveryPhraseEntryDialog_scss_1 = __importDefault(
  require('./WalletRecoveryPhraseEntryDialog.scss')
);
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const mnemonic_input_1 = require('../mnemonic-input');
const messages = (0, react_intl_1.defineMessages)({
  verificationInstructions: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.verification.instructions',
    defaultMessage:
      '!!!Please enter your {wordCount}-word wallet recovery phrase. Make sure you enter the words in the correct order.',
    description:
      'Instructions for verifying wallet recovery phrase on dialog for entering wallet recovery phrase.',
  },
  recoveryPhraseInputLabel: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.recoveryPhraseInputLabel',
    defaultMessage: '!!!Verify your recovery phrase',
    description:
      'Label for the recovery phrase input on dialog for entering wallet recovery phrase.',
  },
  recoveryPhraseInputHint: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.recoveryPhraseInputHint',
    defaultMessage: '!!!Enter your {numberOfWords}-word recovery phrase',
    description: 'Placeholder hint for the mnemonics autocomplete.',
  },
  recoveryPhraseInvalidMnemonics: {
    id:
      'wallet.backup.recovery.phrase.entry.dialog.recoveryPhraseInvalidMnemonics',
    defaultMessage: '!!!Invalid recovery phrase',
    description:
      'Error message shown when invalid recovery phrase was entered.',
  },
  buttonLabelConfirm: {
    id: 'wallet.recovery.phrase.show.entry.dialog.button.labelConfirm',
    defaultMessage: '!!!Confirm',
    description: 'Label for button "Confirm" on wallet backup dialog',
  },
  termOffline: {
    id:
      'wallet.backup.recovery.phrase.entry.dialog.terms.and.condition.offline',
    defaultMessage:
      '!!!I understand that the simplest way to keep my wallet recovery phrase secure is to never store it digitally or online. If I decide to use an online service, such as a password manager with an encrypted database, it is my responsibility to make sure that I use it correctly.',
    description: 'Term on wallet creation to store recovery phrase offline',
  },
  termRecovery: {
    id:
      'wallet.backup.recovery.phrase.entry.dialog.terms.and.condition.recovery',
    defaultMessage:
      '!!!I understand that the only way to recover my wallet if my computer is lost, broken, stolen, or stops working is to use my wallet recovery phrase.',
    description:
      'Term and condition on wallet backup dialog describing that wallet can only be recovered with a security phrase',
  },
});
let WalletRecoveryPhraseEntryDialog = class WalletRecoveryPhraseEntryDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        recoveryPhrase: {
          value: [],
          validators: ({ field }) => {
            const enteredWords = field.value;
            this.props.onUpdateVerificationPhrase({
              verificationPhrase: enteredWords,
            });
            if (this.props.recoveryPhrase !== enteredWords.join(' ')) {
              return this.context.intl.formatMessage(
                messages.recoveryPhraseInvalidMnemonics
              );
            }
            return (0, validations_1.validateMnemonics)({
              requiredWords: cryptoConfig_1.WALLET_RECOVERY_PHRASE_WORD_COUNT,
              providedWords: field.value,
              validator: () => [
                (0, decrypt_1.isValidMnemonic)(
                  enteredWords.join(' '),
                  enteredWords.length
                ),
                this.context.intl.formatMessage(
                  messages.recoveryPhraseInvalidMnemonics
                ),
              ],
            });
          },
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        showErrorsOnChange: false,
        validateOnChange: true,
      },
    }
  );
  handleSubmit = () => {
    this.form.submit({
      onSuccess: () => {
        this.props.onFinishBackup();
      },
    });
  };
  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      enteredPhrase,
      isValid,
      isTermOfflineAccepted,
      isTermRecoveryAccepted,
      isSubmitting,
      onAcceptTermOffline,
      onAcceptTermRecovery,
      canFinishBackup,
      onRestartBackup,
      onCancelBackup,
      recoveryPhrase,
    } = this.props;
    const recoveryPhraseField = form.$('recoveryPhrase');
    const dialogClasses = (0, classnames_1.default)([
      WalletRecoveryPhraseEntryDialog_scss_1.default.component,
      'WalletRecoveryPhraseEntryDialog',
    ]);
    const wordCount = cryptoConfig_1.WALLET_RECOVERY_PHRASE_WORD_COUNT;
    const buttonLabel = !isSubmitting
      ? intl.formatMessage(messages.buttonLabelConfirm)
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const canSubmit =
      (!recoveryPhraseField.error &&
        recoveryPhraseField.value.length === recoveryPhrase.split(' ').length &&
        recoveryPhraseField.value.every((word) => word)) ||
      canFinishBackup;
    const actions = [
      {
        label: buttonLabel,
        onClick: this.handleSubmit,
        disabled: !canSubmit,
        primary: true,
      },
    ];
    const { ...mnemonicInputProps } = recoveryPhraseField.bind();
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
        backButton: !isValid
          ? react_1.default.createElement(DialogBackButton_1.default, {
              onBack: onRestartBackup,
            })
          : null,
      },
      !isValid &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement(WalletRecoveryInstructions_1.default, {
            instructionsText: intl.formatMessage(
              messages.verificationInstructions,
              {
                wordCount,
              }
            ),
          }),
          react_1.default.createElement(mnemonic_input_1.MnemonicInput, {
            ...mnemonicInputProps,
            label: intl.formatMessage(messages.recoveryPhraseInputLabel),
            availableWords: valid_words_en_1.default,
            wordCount: wordCount,
            error: recoveryPhraseField.error,
            reset: form.resetting,
          })
        ),
      isValid &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement(mnemonic_input_1.MnemonicInput, {
            disabled: true,
            value: enteredPhrase,
            wordCount: enteredPhrase.length,
          }),
          react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'div',
              {
                className:
                  WalletRecoveryPhraseEntryDialog_scss_1.default.checkbox,
              },
              react_1.default.createElement(Checkbox_1.Checkbox, {
                label: react_1.default.createElement(
                  react_intl_1.FormattedHTMLMessage,
                  { ...messages.termOffline }
                ),
                onChange: onAcceptTermOffline,
                checked: isTermOfflineAccepted,
                skin: CheckboxSkin_1.CheckboxSkin,
              })
            ),
            react_1.default.createElement(
              'div',
              {
                className:
                  WalletRecoveryPhraseEntryDialog_scss_1.default.checkbox,
              },
              react_1.default.createElement(Checkbox_1.Checkbox, {
                className:
                  WalletRecoveryPhraseEntryDialog_scss_1.default.isBold,
                label: intl.formatMessage(messages.termRecovery),
                onChange: onAcceptTermRecovery,
                checked: isTermRecoveryAccepted,
                skin: CheckboxSkin_1.CheckboxSkin,
              })
            )
          )
        )
    );
  }
};
WalletRecoveryPhraseEntryDialog = __decorate(
  [mobx_react_1.observer],
  WalletRecoveryPhraseEntryDialog
);
exports.default = WalletRecoveryPhraseEntryDialog;
//# sourceMappingURL=WalletRecoveryPhraseEntryDialog.js.map
