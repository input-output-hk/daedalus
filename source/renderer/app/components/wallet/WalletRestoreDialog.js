'use strict';
// @ts-nocheck
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
const lodash_1 = require('lodash');
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const Input_1 = require('@react-polymorph/components/Input');
const react_intl_1 = require('react-intl');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const PasswordInput_1 = require('../widgets/forms/PasswordInput');
const RadioSet_1 = __importDefault(require('../widgets/RadioSet'));
const ReactToolboxMobxForm_1 = __importStar(
  require('../../utils/ReactToolboxMobxForm')
);
const DialogCloseButton_1 = __importDefault(
  require('../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../widgets/Dialog'));
const validations_1 = require('../../utils/validations');
const global_messages_1 = __importDefault(
  require('../../i18n/global-messages')
);
const timingConfig_1 = require('../../config/timingConfig');
const WalletRestoreDialog_scss_1 = __importDefault(
  require('./WalletRestoreDialog.scss')
);
const form_1 = require('../../utils/form');
const walletsConfig_1 = require('../../config/walletsConfig');
const cryptoConfig_1 = require('../../config/cryptoConfig');
const info_icon_inline_svg_1 = __importDefault(
  require('../../assets/images/info-icon.inline.svg')
);
const LoadingSpinner_1 = __importDefault(require('../widgets/LoadingSpinner'));
const mnemonic_input_1 = require('./mnemonic-input');
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.restore.dialog.title.label',
    defaultMessage: '!!!Restore a wallet',
    description: 'Label "Restore wallet" on the wallet restore dialog.',
  },
  walletNameInputLabel: {
    id: 'wallet.restore.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description:
      'Label for the wallet name input on the wallet restore dialog.',
  },
  walletNameInputHint: {
    id: 'wallet.restore.dialog.wallet.name.input.hint',
    defaultMessage: '!!!Name the wallet you are restoring',
    description:
      'Hint "Name the wallet you are restoring" for the wallet name input on the wallet restore dialog.',
  },
  recoveryPhraseTypeLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.type.options.label',
    defaultMessage: '!!!Number of words in the recovery phrase',
    description:
      'Label for the recovery phrase type options on the wallet restore dialog.',
  },
  recoveryPhraseTypeOptionWord: {
    id: 'wallet.restore.dialog.recovery.phrase.type.word',
    defaultMessage: '!!! words',
    description:
      'Word for the recovery phrase type on the wallet restore dialog.',
  },
  recoveryPhraseType15WordOption: {
    id: 'wallet.restore.dialog.recovery.phrase.type.15word.option',
    defaultMessage: '!!!Rewards wallet',
    description:
      'Label for the recovery phrase type 15-word option on the wallet restore dialog.',
  },
  recoveryPhraseType12WordOption: {
    id: 'wallet.restore.dialog.recovery.phrase.type.12word.option',
    defaultMessage: '!!!Balance wallet',
    description:
      'Label for the recovery phrase type 12-word option on the wallet restore dialog.',
  },
  recoveryPhraseInputLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.input.label',
    defaultMessage: '!!!Recovery phrase',
    description:
      'Label for the recovery phrase input on the wallet restore dialog.',
  },
  newLabel: {
    id: 'wallet.restore.dialog.recovery.phrase.newLabel',
    defaultMessage: '!!!New',
    description: 'Label "new" on the wallet restore dialog.',
  },
  importButtonLabel: {
    id: 'wallet.restore.dialog.restore.wallet.button.label',
    defaultMessage: '!!!Restore wallet',
    description:
      'Label for the "Restore wallet" button on the wallet restore dialog.',
  },
  invalidRecoveryPhrase: {
    id: 'wallet.restore.dialog.form.errors.invalidRecoveryPhrase',
    defaultMessage: '!!!Invalid recovery phrase',
    description:
      'Error message shown when invalid recovery phrase was entered.',
  },
  passwordSectionLabel: {
    id: 'wallet.restore.dialog.passwordSectionLabel',
    defaultMessage: '!!!Spending password',
    description: 'Password creation label.',
  },
  passwordSectionDescription: {
    id: 'wallet.restore.dialog.passwordSectionDescription',
    defaultMessage:
      '!!!Keep your wallet secure by setting the spending password',
    description: 'Password creation description.',
  },
  spendingPasswordLabel: {
    id: 'wallet.restore.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Enter password',
    description:
      'Label for the "Wallet password" input in the wallet restore dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.restore.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the wallet restore dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.restore.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the wallet restore dialog.',
  },
  recoveryPhraseTabTitle: {
    id: 'wallet.restore.dialog.tab.title.recoveryPhrase',
    defaultMessage: '!!!Daedalus wallet',
    description: 'Tab title "Daedalus wallet" in the wallet restore dialog.',
  },
  certificateTabTitle: {
    id: 'wallet.restore.dialog.tab.title.certificate',
    defaultMessage: '!!!Daedalus paper wallet',
    description:
      'Tab title "Daedalus paper wallet" in the wallet restore dialog.',
  },
  yoroiTabTitle: {
    id: 'wallet.restore.dialog.tab.title.yoroi',
    defaultMessage: '!!!Yoroi wallet',
    description: 'Tab title "Yoroi wallet" in the wallet restore dialog.',
  },
  shieldedRecoveryPhraseInputLabel: {
    id: 'wallet.restore.dialog.shielded.recovery.phrase.input.label',
    defaultMessage: '!!!27-word paper wallet recovery phrase',
    description:
      'Label for the shielded recovery phrase input on the wallet restore dialog.',
  },
  restorePaperWalletButtonLabel: {
    id: 'wallet.restore.dialog.paper.wallet.button.label',
    defaultMessage: '!!!Restore paper wallet',
    description:
      'Label for the "Restore paper wallet" button on the wallet restore dialog.',
  },
  passwordTooltip: {
    id: 'wallet.dialog.passwordTooltip',
    defaultMessage:
      'We recommend using a password manager app to manage and store your spending password. Generate a unique password using a password manager and paste it here. Passwords should never be reused.',
    description: 'Tooltip for the password input in the wallet dialog.',
  },
});
messages.fieldIsRequired = global_messages_1.default.fieldIsRequired;
let WalletRestoreDialog = class WalletRestoreDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    error: null,
  };
  state = {
    walletType: walletsConfig_1.WALLET_RESTORE_TYPES.LEGACY, // regular | certificate | legacy | yoroi
  };
  recoveryPhraseAutocomplete;
  componentDidUpdate() {
    if (this.props.error) {
      (0, ReactToolboxMobxForm_1.handleFormErrors)(
        '.WalletRestoreDialog_error'
      );
    }
  }
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        walletName: {
          label: this.context.intl.formatMessage(messages.walletNameInputLabel),
          placeholder: this.context.intl.formatMessage(
            messages.walletNameInputHint
          ),
          value: '',
          validators: [
            ({ field }) => [
              (0, validations_1.isValidWalletName)(field.value),
              this.context.intl.formatMessage(
                global_messages_1.default.invalidWalletName
              ),
            ],
          ],
        },
        recoveryPhrase: {
          value: [],
          validators: ({ field }) => {
            const expectedWordCount =
              walletsConfig_1.RECOVERY_PHRASE_WORD_COUNT_OPTIONS[
                this.state.walletType
              ];
            return (0, validations_1.validateMnemonics)({
              requiredWords: expectedWordCount,
              providedWords: field.value,
              validator: (providedWords) => [
                // TODO: we should also validate paper wallets mnemonics here!
                !this.isCertificate()
                  ? this.props.mnemonicValidator(
                      providedWords,
                      expectedWordCount
                    )
                  : true,
                this.context.intl.formatMessage(messages.invalidRecoveryPhrase),
              ],
            });
          },
        },
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
              const repeatPasswordField = form.$('repeatPassword');
              if (repeatPasswordField.value.length > 0) {
                repeatPasswordField.validate({
                  showErrors: true,
                });
              }
              return [
                (0, validations_1.isValidSpendingPassword)(field.value),
                this.context.intl.formatMessage(
                  global_messages_1.default.invalidSpendingPassword
                ),
              ];
            },
          ],
        },
        repeatPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
              const spendingPassword = form.$('spendingPassword').value;
              if (spendingPassword.length === 0) return [true];
              return [
                (0, validations_1.isValidRepeatPassword)(
                  spendingPassword,
                  field.value
                ),
                this.context.intl.formatMessage(
                  global_messages_1.default.invalidRepeatPassword
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
  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { onSubmit } = this.props;
        const { recoveryPhrase, walletName, spendingPassword } = form.values();
        const walletData = {
          recoveryPhrase: (0, lodash_1.join)(recoveryPhrase, ' '),
          walletName,
          spendingPassword,
        };
        walletData.type = this.state.walletType;
        onSubmit(walletData);
      },
      onError: () =>
        (0, ReactToolboxMobxForm_1.handleFormErrors)('.SimpleFormField_error', {
          focusElement: true,
        }),
    });
  };
  handleSubmitOnEnter = form_1.submitOnEnter.bind(this, this.submit);
  resetForm = () => {
    const { form } = this;
    // Cancel all debounced field validations
    form.each((field) => {
      field.debouncedValidation.cancel();
    });
    form.reset();
    form.showErrors(false);
  };
  resetMnemonics = () => {
    const recoveryPhraseField = this.form.$('recoveryPhrase');
    recoveryPhraseField.debouncedValidation.cancel();
    recoveryPhraseField.reset();
    recoveryPhraseField.showErrors(false);
  };
  render() {
    const { intl } = this.context;
    const { form } = this;
    const { walletType } = this.state;
    const { suggestedMnemonics, isSubmitting, error, onCancel } = this.props;
    const dialogClasses = (0, classnames_1.default)([
      WalletRestoreDialog_scss_1.default.component,
      'WalletRestoreDialog',
    ]);
    const walletNameFieldClasses = (0, classnames_1.default)([
      'walletName',
      WalletRestoreDialog_scss_1.default.walletName,
    ]);
    const walletNameField = form.$('walletName');
    const recoveryPhraseField = form.$('recoveryPhrase');
    const spendingPasswordField = form.$('spendingPassword');
    const repeatedPasswordField = form.$('repeatPassword');
    const label = this.isCertificate()
      ? this.context.intl.formatMessage(messages.restorePaperWalletButtonLabel)
      : this.context.intl.formatMessage(messages.importButtonLabel);
    const buttonLabel = !isSubmitting
      ? label
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const actions = [
      {
        label: buttonLabel,
        primary: true,
        disabled: isSubmitting,
        onClick: this.submit,
      },
    ];
    const regularTabClasses = (0, classnames_1.default)([
      'regularTab',
      this.isRegular() || this.isLegacy()
        ? WalletRestoreDialog_scss_1.default.activeButton
        : '',
    ]);
    const certificateTabClasses = (0, classnames_1.default)([
      'certificateTab',
      this.isCertificate()
        ? WalletRestoreDialog_scss_1.default.activeButton
        : '',
    ]);
    const yoroiTabClasses = (0, classnames_1.default)([
      'yoroiTab',
      this.isYoroi() ? WalletRestoreDialog_scss_1.default.activeButton : '',
    ]);
    const { ...mnemonicInputProps } = recoveryPhraseField.bind();
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogClasses,
        title: intl.formatMessage(messages.title),
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onCancel,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(
        'div',
        { className: WalletRestoreDialog_scss_1.default.restoreTypeChoice },
        react_1.default.createElement(
          'button',
          {
            className: regularTabClasses,
            onClick: () =>
              this.onSelectWalletType(
                walletsConfig_1.WALLET_RESTORE_TYPES.LEGACY,
                true
              ),
          },
          intl.formatMessage(messages.recoveryPhraseTabTitle)
        ),
        react_1.default.createElement(
          'button',
          {
            className: certificateTabClasses,
            onClick: () =>
              this.onSelectWalletType(
                walletsConfig_1.WALLET_RESTORE_TYPES.CERTIFICATE,
                true
              ),
          },
          intl.formatMessage(messages.certificateTabTitle)
        ),
        react_1.default.createElement(
          'button',
          {
            className: yoroiTabClasses,
            onClick: () =>
              this.onSelectWalletType(
                walletsConfig_1.WALLET_RESTORE_TYPES.YOROI_LEGACY,
                true
              ),
          },
          intl.formatMessage(messages.yoroiTabTitle)
        )
      ),
      react_1.default.createElement(Input_1.Input, {
        className: walletNameFieldClasses,
        onKeyPress: this.handleSubmitOnEnter,
        ...walletNameField.bind(),
        error: walletNameField.error,
      }),
      (this.isRegular() || this.isLegacy()) &&
        react_1.default.createElement(RadioSet_1.default, {
          label: intl.formatMessage(messages.recoveryPhraseTypeLabel),
          items: [
            {
              key: walletsConfig_1.WALLET_RESTORE_TYPES.LEGACY,
              label: react_1.default.createElement(
                react_1.Fragment,
                null,
                cryptoConfig_1.LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
                intl.formatMessage(messages.recoveryPhraseTypeOptionWord),
                ' ',
                react_1.default.createElement(
                  'span',
                  null,
                  '(',
                  intl.formatMessage(messages.recoveryPhraseType12WordOption),
                  ')'
                )
              ),
              selected: this.isLegacy(),
              onChange: () =>
                this.onSelectWalletType(
                  walletsConfig_1.WALLET_RESTORE_TYPES.LEGACY
                ),
            },
            {
              key: walletsConfig_1.WALLET_RESTORE_TYPES.REGULAR,
              label: react_1.default.createElement(
                react_1.Fragment,
                null,
                cryptoConfig_1.WALLET_RECOVERY_PHRASE_WORD_COUNT,
                intl.formatMessage(messages.recoveryPhraseTypeOptionWord),
                ' ',
                react_1.default.createElement(
                  'span',
                  null,
                  '(',
                  intl.formatMessage(messages.recoveryPhraseType15WordOption),
                  ')'
                ),
                react_1.default.createElement(
                  'span',
                  { className: WalletRestoreDialog_scss_1.default.newLabel },
                  intl.formatMessage(messages.newLabel)
                )
              ),
              selected: !this.isLegacy(),
              onChange: () =>
                this.onSelectWalletType(
                  walletsConfig_1.WALLET_RESTORE_TYPES.REGULAR
                ),
            },
          ],
        }),
      this.isYoroi() &&
        react_1.default.createElement(RadioSet_1.default, {
          label: intl.formatMessage(messages.recoveryPhraseTypeLabel),
          items: [
            {
              key: walletsConfig_1.WALLET_RESTORE_TYPES.YOROI_LEGACY,
              label: react_1.default.createElement(
                react_1.Fragment,
                null,
                cryptoConfig_1.YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT,
                intl.formatMessage(messages.recoveryPhraseTypeOptionWord),
                ' ',
                react_1.default.createElement(
                  'span',
                  null,
                  '(',
                  intl.formatMessage(messages.recoveryPhraseType12WordOption),
                  ')'
                )
              ),
              selected: this.isYoroiLegacy(),
              onChange: () =>
                this.onSelectWalletType(
                  walletsConfig_1.WALLET_RESTORE_TYPES.YOROI_LEGACY
                ),
            },
            {
              key: walletsConfig_1.WALLET_RESTORE_TYPES.YOROI_REGULAR,
              label: react_1.default.createElement(
                react_1.Fragment,
                null,
                cryptoConfig_1.YOROI_WALLET_RECOVERY_PHRASE_WORD_COUNT,
                intl.formatMessage(messages.recoveryPhraseTypeOptionWord),
                ' ',
                react_1.default.createElement(
                  'span',
                  null,
                  '(',
                  intl.formatMessage(messages.recoveryPhraseType15WordOption),
                  ')'
                ),
                react_1.default.createElement(
                  'span',
                  { className: WalletRestoreDialog_scss_1.default.newLabel },
                  intl.formatMessage(messages.newLabel)
                )
              ),
              selected: this.isYoroiRegular(),
              onChange: () =>
                this.onSelectWalletType(
                  walletsConfig_1.WALLET_RESTORE_TYPES.YOROI_REGULAR
                ),
            },
          ],
        }),
      react_1.default.createElement(mnemonic_input_1.MnemonicInput, {
        ...mnemonicInputProps,
        label: this.isCertificate()
          ? intl.formatMessage(messages.shieldedRecoveryPhraseInputLabel)
          : intl.formatMessage(messages.recoveryPhraseInputLabel),
        availableWords: suggestedMnemonics,
        wordCount:
          walletsConfig_1.RECOVERY_PHRASE_WORD_COUNT_OPTIONS[walletType],
        error: recoveryPhraseField.error,
        reset: form.resetting,
      }),
      react_1.default.createElement(
        'div',
        {
          className: WalletRestoreDialog_scss_1.default.spendingPasswordWrapper,
        },
        react_1.default.createElement(
          'div',
          {
            className: WalletRestoreDialog_scss_1.default.passwordSectionLabel,
          },
          intl.formatMessage(messages.passwordSectionLabel)
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              WalletRestoreDialog_scss_1.default.passwordSectionDescription,
          },
          intl.formatMessage(messages.passwordSectionDescription)
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              WalletRestoreDialog_scss_1.default.spendingPasswordFields,
          },
          react_1.default.createElement(
            'div',
            {
              className:
                WalletRestoreDialog_scss_1.default.spendingPasswordField,
            },
            react_1.default.createElement(PasswordInput_1.PasswordInput, {
              className: 'spendingPassword',
              onKeyPress: this.handleSubmitOnEnter,
              ...spendingPasswordField.bind(),
            }),
            react_1.default.createElement(
              PopOver_1.PopOver,
              {
                content: react_1.default.createElement(
                  react_intl_1.FormattedHTMLMessage,
                  { ...messages.passwordTooltip }
                ),
                key: 'tooltip',
              },
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: info_icon_inline_svg_1.default,
                className: WalletRestoreDialog_scss_1.default.infoIcon,
              })
            )
          ),
          react_1.default.createElement(
            'div',
            {
              className:
                WalletRestoreDialog_scss_1.default.spendingPasswordField,
            },
            react_1.default.createElement(PasswordInput_1.PasswordInput, {
              className: 'repeatedPassword',
              onKeyPress: this.handleSubmitOnEnter,
              ...repeatedPasswordField.bind(),
              repeatPassword: spendingPasswordField.value,
              isPasswordRepeat: true,
            })
          )
        ),
        react_1.default.createElement(
          'p',
          {
            className: WalletRestoreDialog_scss_1.default.passwordInstructions,
          },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...global_messages_1.default.passwordInstructions,
          })
        )
      ),
      error &&
        react_1.default.createElement(
          'p',
          { className: WalletRestoreDialog_scss_1.default.error },
          intl.formatMessage(error)
        )
    );
  }
  isRegular() {
    return (
      this.state.walletType === walletsConfig_1.WALLET_RESTORE_TYPES.REGULAR
    );
  }
  isCertificate() {
    return (
      this.state.walletType === walletsConfig_1.WALLET_RESTORE_TYPES.CERTIFICATE
    );
  }
  isLegacy() {
    return (
      this.state.walletType === walletsConfig_1.WALLET_RESTORE_TYPES.LEGACY
    );
  }
  isYoroiLegacy() {
    return (
      this.state.walletType ===
      walletsConfig_1.WALLET_RESTORE_TYPES.YOROI_LEGACY
    );
  }
  isYoroiRegular() {
    return (
      this.state.walletType ===
      walletsConfig_1.WALLET_RESTORE_TYPES.YOROI_REGULAR
    );
  }
  isYoroi() {
    return this.isYoroiLegacy() || this.isYoroiRegular();
  }
  onSelectWalletType = (walletType, shouldResetForm) => {
    const { onChoiceChange, isSubmitting } = this.props;
    if (isSubmitting) return;
    this.setState({
      walletType,
    });
    if (shouldResetForm) this.resetForm();
    this.resetMnemonics();
    if (onChoiceChange) onChoiceChange();
  };
};
WalletRestoreDialog = __decorate([mobx_react_1.observer], WalletRestoreDialog);
exports.default = WalletRestoreDialog;
//# sourceMappingURL=WalletRestoreDialog.js.map
