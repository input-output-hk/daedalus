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
const react_intl_1 = require('react-intl');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const validations_1 = require('../../../utils/validations');
const WalletRestoreDialog_1 = __importDefault(
  require('./widgets/WalletRestoreDialog')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const valid_words_en_1 = __importDefault(
  require('../../../../../common/config/crypto/valid-words.en')
);
const mnemonic_input_1 = require('../mnemonic-input');
const messages = (0, react_intl_1.defineMessages)({
  autocompletePlaceholder: {
    id: 'wallet.restore.dialog.step.mnemonics.autocomplete.placeholder',
    defaultMessage: '!!!Enter word #{wordNumber}',
    description: 'Placeholder for the mnemonics autocomplete.',
  },
  autocompleteMultiLengthPhrase: {
    id:
      'wallet.restore.dialog.step.mnemonics.autocomplete.multiLengthPhrase.placeholder',
    defaultMessage: '!!!Enter your 12, 18 or 24-word recovery phrase',
    description: 'Placeholder for the multi-length mnemonics autocomplete.',
  },
  autocompleteNoResults: {
    id: 'wallet.restore.dialog.step.mnemonics.autocomplete.noResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the mnemonics autocomplete search results.',
  },
  continueButtonLabel: {
    id: 'wallet.restore.dialog.step.mnemonics.autocomplete.continueButtonLabel',
    defaultMessage: '!!!Check recovery phrase',
    description: 'Label for the mnemonics Continue button.',
  },
  invalidRecoveryPhrase: {
    id:
      'wallet.restore.dialog.step.mnemonics.autocomplete.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description: 'Label for invalid recovery phrase',
  },
});
let MnemonicsDialog = class MnemonicsDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        recoveryPhrase: {
          value: [...this.props.mnemonics],
          validators: ({ field }) =>
            (0, validations_1.validateMnemonics)({
              requiredWords: this.props.expectedWordCount,
              providedWords: field.value,
              validator: (enteredWords) => [
                this.props.onValidateMnemonics(enteredWords),
                this.context.intl.formatMessage(messages.invalidRecoveryPhrase),
              ],
            }),
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
  submit = () => {
    this.form.submit({
      onSuccess: this.props.onContinue,
      onError: () => {},
    });
  };
  render() {
    const { intl } = this.context;
    const { onClose, onBack, onSetWalletMnemonics, maxWordCount } = this.props;
    const recoveryPhraseField = this.form.$('recoveryPhrase');
    const canSubmit =
      !recoveryPhraseField.error &&
      recoveryPhraseField.value.length === maxWordCount &&
      recoveryPhraseField.value.every((word) => word);
    const { ...mnemonicInputProps } = recoveryPhraseField.bind();
    return react_1.default.createElement(
      WalletRestoreDialog_1.default,
      {
        stepNumber: 1,
        actions: [
          {
            primary: true,
            disabled: !canSubmit,
            label: intl.formatMessage(messages.continueButtonLabel),
            onClick: this.submit,
          },
        ],
        onClose: onClose,
        onBack: onBack,
      },
      react_1.default.createElement(mnemonic_input_1.MnemonicInput, {
        ...mnemonicInputProps,
        label: intl.formatMessage(
          global_messages_1.default.recoveryPhraseDialogTitle
        ),
        onChange: (enteredMnemonics) => {
          recoveryPhraseField.set(enteredMnemonics);
          onSetWalletMnemonics(enteredMnemonics);
        },
        availableWords: valid_words_en_1.default,
        wordCount: maxWordCount,
        error: recoveryPhraseField.error,
        reset: this.form.resetting,
      })
    );
  }
};
MnemonicsDialog = __decorate([mobx_react_1.observer], MnemonicsDialog);
exports.default = MnemonicsDialog;
//# sourceMappingURL=MnemonicsDialog.js.map
