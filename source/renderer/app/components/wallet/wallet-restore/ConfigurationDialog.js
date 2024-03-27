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
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const Input_1 = require('@react-polymorph/components/Input');
const react_intl_1 = require('react-intl');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const PasswordInput_1 = require('../../widgets/forms/PasswordInput');
const WalletRestoreDialog_1 = __importDefault(
  require('./widgets/WalletRestoreDialog')
);
const ConfigurationDialog_scss_1 = __importDefault(
  require('./ConfigurationDialog.scss')
);
const ReactToolboxMobxForm_1 = __importStar(
  require('../../../utils/ReactToolboxMobxForm')
);
const validations_1 = require('../../../utils/validations');
const form_1 = require('../../../utils/form');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const timingConfig_1 = require('../../../config/timingConfig');
const info_icon_inline_svg_1 = __importDefault(
  require('../../../assets/images/info-icon.inline.svg')
);
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const messages = (0, react_intl_1.defineMessages)({
  description1: {
    id: 'wallet.restore.dialog.step.configuration.description1',
    defaultMessage:
      '!!!Name your restored wallet and set a spending password to keep your wallet secure.',
    description: 'Description1 for Configuration Step',
  },
  description2: {
    id: 'wallet.restore.dialog.step.configuration.description2',
    defaultMessage:
      '!!!Wallet names and spending passwords are only stored locally and are not stored on the blockchain. You can give your restored wallet a new name and set a new spending password, you don’t need to match the wallet name and spending password you were using before. <b>Only the recovery phrase from your original wallet is needed to restore a wallet.</b>',
    description: 'Description2 for Configuration Step',
  },
  walletNameLabel: {
    id: 'wallet.restore.dialog.step.configuration.input.walletName.label',
    defaultMessage: '!!!Wallet name',
    description: 'Label for Wallet Name Input',
  },
  walletNamePlaceholder: {
    id: 'wallet.restore.dialog.step.configuration.input.walletName.placeholder',
    defaultMessage: '!!!Name the wallet you are restoring',
    description: 'Placeholder for Wallet Name Input',
  },
  spendingPasswordLabel: {
    id: 'wallet.restore.dialog.step.configuration.input.spendingPassword.label',
    defaultMessage: '!!!Enter password',
    description:
      'Label for the "Wallet password" input in the wallet restore dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.restore.dialog.step.configuration.input.repeatPassword.label',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the wallet restore dialog.',
  },
  passwordFieldsPlaceholder: {
    id:
      'wallet.restore.dialog.step.configuration.input.passwordFields.placeholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the wallet restore dialog.',
  },
  continueButtonLabel: {
    id: 'wallet.restore.dialog.step.configuration.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description: 'Placeholder for the dialog "Continue" button',
  },
  passwordTooltip: {
    id: 'wallet.dialog.passwordTooltip',
    defaultMessage:
      '!!!It is really good to use Password Manager apps to improve security. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris mattis diam non nulla sollicitudin, ac ultrices purus luctus.',
    description: 'Tooltip for the password input in the create wallet dialog.',
  },
});
let ConfigurationDialog = class ConfigurationDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    error: null,
  };
  componentDidUpdate() {
    if (this.props.error) {
      (0, ReactToolboxMobxForm_1.handleFormErrors)(
        '.ConfigurationDialog_error'
      );
    }
  }
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        walletName: {
          label: this.context.intl.formatMessage(messages.walletNameLabel),
          placeholder: this.context.intl.formatMessage(
            messages.walletNamePlaceholder
          ),
          value: this.props.walletName,
          validators: [
            ({ field }) => [
              (0, validations_1.isValidWalletName)(field.value),
              this.context.intl.formatMessage(
                global_messages_1.default.invalidWalletName
              ),
            ],
          ],
          hooks: {
            onChange: this.props.onChange.bind(this, 'walletName'),
          },
        },
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldsPlaceholder
          ),
          value: this.props.spendingPassword,
          validators: [
            ({ field, form }) => {
              const repeatPasswordField = form.$('repeatPassword');
              const isRepeatPasswordFieldSet =
                repeatPasswordField.value.length > 0;
              repeatPasswordField.validate({
                showErrors: isRepeatPasswordFieldSet,
              });
              return [
                (0, validations_1.isValidSpendingPassword)(field.value),
                this.context.intl.formatMessage(
                  global_messages_1.default.invalidSpendingPassword
                ),
              ];
            },
          ],
          hooks: {
            onChange: this.props.onChange.bind(this, 'spendingPassword'),
          },
        },
        repeatPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldsPlaceholder
          ),
          value: this.props.repeatPassword,
          validators: [
            ({ field, form }) => {
              const spendingPassword = form.$('spendingPassword').value;
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
          hooks: {
            onChange: this.props.onChange.bind(this, 'repeatPassword'),
          },
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { onContinue } = this.props;
        const { walletName, spendingPassword } = form.values();
        onContinue(walletName, spendingPassword);
      },
      onError: () =>
        (0, ReactToolboxMobxForm_1.handleFormErrors)(
          '.ConfigurationDialog_error',
          {
            focusElement: true,
          }
        ),
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
  render() {
    const { intl } = this.context;
    const { onClose, onBack, error, isSubmitting, currentLocale } = this.props;
    const { form } = this;
    const walletNameField = form.$('walletName');
    const spendingPasswordField = form.$('spendingPassword');
    const repeatPasswordField = form.$('repeatPassword');
    const walletNameFieldClasses = (0, classnames_1.default)([
      ConfigurationDialog_scss_1.default.walletName,
      'walletName',
    ]);
    const spendingPasswordClasses = (0, classnames_1.default)([
      ConfigurationDialog_scss_1.default.spendingPasswordField,
      currentLocale === 'ja-JP'
        ? ConfigurationDialog_scss_1.default.jpLangTooltipIcon
        : '',
    ]);
    const buttonLabel = !isSubmitting
      ? intl.formatMessage(messages.continueButtonLabel)
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const canSubmit = !isSubmitting && form.isValid;
    return react_1.default.createElement(
      WalletRestoreDialog_1.default,
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      {
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        className: ConfigurationDialog_scss_1.default.dialogComponent,
        stepNumber: 2,
        actions: [
          {
            disabled: !canSubmit,
            primary: true,
            label: buttonLabel,
            onClick: this.submit,
          },
        ],
        onClose: onClose,
        onBack: onBack,
      },
      react_1.default.createElement(
        'div',
        { className: ConfigurationDialog_scss_1.default.component },
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(messages.description1)
        ),
        react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.description2,
          })
        ),
        react_1.default.createElement(Input_1.Input, {
          className: walletNameFieldClasses,
          onKeyPress: this.handleSubmitOnEnter,
          ...walletNameField.bind(),
          error: walletNameField.error,
        }),
        react_1.default.createElement(
          'div',
          null,
          react_1.default.createElement(
            'div',
            {
              className:
                ConfigurationDialog_scss_1.default.spendingPasswordFields,
            },
            react_1.default.createElement(
              'div',
              { className: spendingPasswordClasses },
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
                  className: ConfigurationDialog_scss_1.default.infoIcon,
                })
              )
            ),
            react_1.default.createElement(
              'div',
              {
                className:
                  ConfigurationDialog_scss_1.default.spendingPasswordField,
              },
              react_1.default.createElement(PasswordInput_1.PasswordInput, {
                className: 'repeatPassword',
                onKeyPress: this.handleSubmitOnEnter,
                ...repeatPasswordField.bind(),
                repeatPassword: spendingPasswordField.value,
                isPasswordRepeat: true,
              })
            )
          ),
          react_1.default.createElement(
            'div',
            {
              className:
                ConfigurationDialog_scss_1.default.passwordInstructions,
            },
            react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
              ...global_messages_1.default.passwordInstructions,
            })
          )
        ),
        error &&
          react_1.default.createElement(
            'p',
            { className: ConfigurationDialog_scss_1.default.error },
            intl.formatMessage(error)
          )
      )
    );
  }
};
ConfigurationDialog = __decorate([mobx_react_1.observer], ConfigurationDialog);
exports.default = ConfigurationDialog;
//# sourceMappingURL=ConfigurationDialog.js.map
