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
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const errors_1 = require('../../../i18n/errors');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const VerificationDialog_scss_1 = __importDefault(
  require('./VerificationDialog.scss')
);
const cryptoConfig_1 = require('../../../config/cryptoConfig');
const timingConfig_1 = require('../../../config/timingConfig');
const mnemonic_input_1 = require('../mnemonic-input');
const messages = (0, react_intl_1.defineMessages)({
  headline: {
    id: 'paper.wallet.create.certificate.verification.dialog.headline',
    defaultMessage: '!!!Verify certificate',
    description:
      'Headline for the "Paper wallet create certificate verification dialog".',
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.verification.dialog.subtitle',
    defaultMessage:
      '!!!Enter your paper wallet recovery phrase to verify your paper wallet certificate.',
    description:
      '"Paper wallet create certificate verification dialog" subtitle.',
  },
  instructions: {
    id: 'paper.wallet.create.certificate.verification.dialog.instructions',
    defaultMessage: `!!!Make sure you enter all {fullPhraseWordCount} words for the paper wallet recovery phrase,
     first {printedWordCount} words printed on the certificate followed by the {writtenWordCount} words you wrote by hand.`,
    description:
      '"Paper wallet create certificate verification dialog" subtitle.',
  },
  recoveryPhraseLabel: {
    id:
      'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.label',
    defaultMessage: '!!!Paper wallet recovery phrase',
    description:
      '"Paper wallet create certificate verification dialog" recovery phrase label.',
  },
  clearButtonLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.button.clearLabel',
    defaultMessage: '!!!Clear',
    description:
      '"Paper wallet create certificate verification dialog" button clear label.',
  },
  storingUnderstandanceLabel: {
    id:
      'paper.wallet.create.certificate.verification.dialog.storingUnderstandanceConfirmationLabel',
    defaultMessage:
      '!!!I understand that the paper wallet I create will not be stored in Daedalus.',
    description:
      '"Paper wallet create certificate verification dialog" storing understandance confirmation.',
  },
  recoveringUnderstandanceLabel: {
    id:
      'paper.wallet.create.certificate.verification.dialog.recoveringUnderstandanceConfirmationLabel',
    defaultMessage:
      '!!!I understand that my paper wallet can be recovered only by using my paper wallet certificate.',
    description:
      '"Paper wallet create certificate verification dialog" recovering understandance confirmation.',
  },
});
let VerificationDialog = class VerificationDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    storingConfirmed: false,
    recoveringConfirmed: false,
    isRecoveryPhraseValid: false,
    error: false,
  };
  onStoringConfirmation = () => {
    this.setState((prevState) => ({
      storingConfirmed: !prevState.storingConfirmed,
    }));
  };
  onRecoveringConfirmation = () => {
    this.setState((prevState) => ({
      recoveringConfirmed: !prevState.recoveringConfirmed,
    }));
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        recoveryPhrase: {
          label: this.context.intl.formatMessage(messages.recoveryPhraseLabel),
          value: [],
          validators: [
            ({ field }) => {
              const { intl } = this.context;
              const {
                walletCertificateRecoveryPhrase,
                additionalMnemonicWords,
              } = this.props;
              const { storingConfirmed, recoveringConfirmed } = this.state;
              const enteredWordsArray = field.value;
              if (
                enteredWordsArray.length <
                cryptoConfig_1.PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT
              ) {
                // If user hasn't entered all words of the paper wallet recovery phrase yet
                return [
                  false,
                  intl.formatMessage(
                    global_messages_1.default.incompleteMnemonic,
                    {
                      expected:
                        cryptoConfig_1.PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
                    }
                  ),
                ];
              }
              const fullRecoveryPhrase = `${walletCertificateRecoveryPhrase} ${additionalMnemonicWords}`;
              const enteredRecoveryPhrase = enteredWordsArray.join(' ');
              const isRecoveryPhraseValid =
                fullRecoveryPhrase === enteredRecoveryPhrase;
              this.setState({
                isRecoveryPhraseValid,
                // disabled and uncheck confirmation checkboxes if recovery phrase is not valid
                storingConfirmed: isRecoveryPhraseValid
                  ? storingConfirmed
                  : false,
                recoveringConfirmed: isRecoveryPhraseValid
                  ? recoveringConfirmed
                  : false,
              });
              return [
                isRecoveryPhraseValid,
                this.context.intl.formatMessage(
                  new errors_1.InvalidMnemonicError()
                ),
              ];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        validateOnChange: true,
        showErrorsOnChange: false,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  handleMnemonicInputChange = (value) => {
    if (this.state.error) {
      this.setState((prevState) => ({ ...prevState, error: false }));
    }
    this.form.$('recoveryPhrase').bind().onChange(value);
  };
  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { recoveryPhrase } = form.values();
        this.props.onContinue({
          recoveryPhrase,
        });
      },
      onError: () => {
        this.setState((prevState) => ({ ...prevState, error: true }));
      },
    });
  };
  resetForm = () => {
    const { form } = this;
    // Cancel all debounced field validations
    form.each((field) => {
      field.debouncedValidation.cancel();
    });
    form.reset();
    form.showErrors(false);
    this.setState({
      storingConfirmed: false,
      recoveringConfirmed: false,
    });
  };
  render() {
    const { intl } = this.context;
    const { form, resetForm } = this;
    const { suggestedMnemonics, onClose } = this.props;
    const {
      storingConfirmed,
      recoveringConfirmed,
      isRecoveryPhraseValid,
    } = this.state;
    const recoveryPhraseField = form.$('recoveryPhrase');
    const dialogClasses = (0, classnames_1.default)([
      VerificationDialog_scss_1.default.dialog,
      'verificationDialog',
    ]);
    const storingUnderstandanceCheckboxClasses = (0, classnames_1.default)([
      VerificationDialog_scss_1.default.checkbox,
      'storingUnderstandance',
    ]);
    const recoveringUnderstandanceCheckboxClasses = (0, classnames_1.default)([
      VerificationDialog_scss_1.default.checkbox,
      'recoveringUnderstandance',
    ]);
    const actions = [
      {
        className: 'clearButton',
        label: intl.formatMessage(messages.clearButtonLabel),
        onClick: resetForm.bind(this),
      },
      {
        className: 'continueButton',
        label: intl.formatMessage(
          global_messages_1.default.dialogButtonContinueLabel
        ),
        primary: true,
        disabled: form.hasError,
        onClick: this.submit.bind(this),
      },
    ];
    const { ...mnemonicInputProps } = recoveryPhraseField.bind();
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogClasses,
        title: intl.formatMessage(messages.headline),
        actions: actions,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(
        'div',
        {
          className:
            VerificationDialog_scss_1.default.verificationContentWrapper,
        },
        react_1.default.createElement(
          'p',
          { className: VerificationDialog_scss_1.default.subtitle },
          intl.formatMessage(messages.subtitle)
        ),
        react_1.default.createElement(
          'p',
          { className: VerificationDialog_scss_1.default.instructions },
          react_1.default.createElement(
            'strong',
            null,
            intl.formatMessage(messages.instructions, {
              fullPhraseWordCount:
                cryptoConfig_1.PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
              printedWordCount: cryptoConfig_1.PAPER_WALLET_PRINTED_WORDS_COUNT,
              writtenWordCount: cryptoConfig_1.PAPER_WALLET_WRITTEN_WORDS_COUNT,
            })
          )
        ),
        react_1.default.createElement(
          'div',
          { className: VerificationDialog_scss_1.default.content },
          react_1.default.createElement(mnemonic_input_1.MnemonicInput, {
            ...mnemonicInputProps,
            onChange: this.handleMnemonicInputChange,
            availableWords: suggestedMnemonics.sort(),
            wordCount: cryptoConfig_1.PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
            error: recoveryPhraseField.error,
            reset: form.resetting,
          }),
          react_1.default.createElement(Checkbox_1.Checkbox, {
            className: storingUnderstandanceCheckboxClasses,
            label: intl.formatMessage(messages.storingUnderstandanceLabel),
            onChange: this.onStoringConfirmation,
            checked: storingConfirmed,
            disabled: !isRecoveryPhraseValid,
            skin: CheckboxSkin_1.CheckboxSkin,
          }),
          react_1.default.createElement(Checkbox_1.Checkbox, {
            className: recoveringUnderstandanceCheckboxClasses,
            label: intl.formatMessage(messages.recoveringUnderstandanceLabel),
            onChange: this.onRecoveringConfirmation,
            checked: recoveringConfirmed,
            disabled: !isRecoveryPhraseValid,
            skin: CheckboxSkin_1.CheckboxSkin,
          })
        )
      )
    );
  }
};
VerificationDialog = __decorate([mobx_react_1.observer], VerificationDialog);
exports.default = VerificationDialog;
//# sourceMappingURL=VerificationDialog.js.map
