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
// TODO: Remove once the new wallet creation process is ready
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const react_intl_1 = require('react-intl');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
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
const PasswordInput_1 = require('../widgets/forms/PasswordInput');
const WalletCreateDialog_scss_1 = __importDefault(
  require('./WalletCreateDialog.scss')
);
const timingConfig_1 = require('../../config/timingConfig');
const form_1 = require('../../utils/form');
const info_icon_inline_svg_1 = __importDefault(
  require('../../assets/images/info-icon.inline.svg')
);
const LoadingSpinner_1 = __importDefault(require('../widgets/LoadingSpinner'));
const messages = (0, react_intl_1.defineMessages)({
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create form.',
  },
  walletName: {
    id: 'wallet.create.dialog.name.label',
    defaultMessage: '!!!Wallet name',
    description:
      'Label for the "Wallet Name" text input in the wallet create form.',
  },
  walletNameHint: {
    id: 'wallet.create.dialog.walletNameHint',
    defaultMessage: '!!!Enter wallet name',
    description:
      'Hint for the "Wallet Name" text input in the wallet create form.',
  },
  createPersonalWallet: {
    id: 'wallet.create.dialog.create.personal.wallet.button.label',
    defaultMessage: '!!!Create Shelley wallet',
    description:
      'Label for the "Create Shelley wallet" button on create wallet dialog.',
  },
  passwordSectionLabel: {
    id: 'wallet.create.dialog.passwordSectionLabel',
    defaultMessage: '!!!Spending password',
    description: 'Password creation label.',
  },
  passwordSectionDescription: {
    id: 'wallet.create.dialog.passwordSectionDescription',
    defaultMessage: '!!!Keep your wallet secure by setting a spending password',
    description: 'Password creation description.',
  },
  spendingPasswordLabel: {
    id: 'wallet.create.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Enter password',
    description:
      'Label for the "Wallet password" input in the create wallet dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.create.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the create wallet dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.create.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the create wallet dialog.',
  },
  passwordTooltip: {
    id: 'wallet.dialog.passwordTooltip',
    defaultMessage:
      'We recommend using a password manager app to manage and store your spending password. Generate a unique password using a password manager and paste it here. Passwords should never be reused.',
    description: 'Tooltip for the password input in the wallet dialog.',
  },
});
let WalletCreateDialog = class WalletCreateDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    isSubmitting: false,
  };
  componentDidMount() {
    setTimeout(() => {
      this.walletNameInput.focus();
    });
  }
  walletNameInput;
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        walletName: {
          label: this.context.intl.formatMessage(messages.walletName),
          placeholder: this.context.intl.formatMessage(messages.walletNameHint),
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
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    this.setState({
      isSubmitting: false,
    });
    this.form.submit({
      onSuccess: (form) => {
        this.setState({
          isSubmitting: true,
        });
        const { walletName, spendingPassword } = form.values();
        const walletData = {
          name: walletName,
          spendingPassword,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {
        (0, ReactToolboxMobxForm_1.handleFormErrors)('.SimpleFormField_error', {
          focusElement: true,
        });
        this.setState({
          isSubmitting: false,
        });
      },
    });
  };
  handleSubmitOnEnter = form_1.submitOnEnter.bind(this, this.submit);
  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onCancel, currentLocale } = this.props;
    const { isSubmitting } = this.state;
    const dialogClasses = (0, classnames_1.default)([
      WalletCreateDialog_scss_1.default.component,
      'WalletCreateDialog',
    ]);
    const spendingPasswordClasses = (0, classnames_1.default)([
      WalletCreateDialog_scss_1.default.spendingPasswordField,
      currentLocale === 'ja-JP'
        ? WalletCreateDialog_scss_1.default.jpLangTooltipIcon
        : '',
    ]);
    const walletNameField = form.$('walletName');
    const spendingPasswordField = form.$('spendingPassword');
    const repeatedPasswordField = form.$('repeatPassword');
    const canSubmit = !isSubmitting && form.isValid;
    const buttonLabel = !isSubmitting
      ? this.context.intl.formatMessage(messages.createPersonalWallet)
      : react_1.default.createElement(LoadingSpinner_1.default, null);
    const actions = [
      {
        disabled: !canSubmit,
        label: buttonLabel,
        primary: true,
        onClick: this.submit,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogClasses,
        title: intl.formatMessage(messages.dialogTitle),
        actions: actions,
        closeOnOverlayClick: true,
        onClose: !isSubmitting ? onCancel : () => {},
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(Input_1.Input, {
        className: 'walletName',
        onKeyPress: this.handleSubmitOnEnter,
        ref: (input) => {
          this.walletNameInput = input;
        },
        ...walletNameField.bind(),
        error: walletNameField.error,
        skin: InputSkin_1.InputSkin,
      }),
      react_1.default.createElement(
        'div',
        {
          className: WalletCreateDialog_scss_1.default.spendingPasswordWrapper,
        },
        react_1.default.createElement(
          'div',
          { className: WalletCreateDialog_scss_1.default.passwordSectionLabel },
          intl.formatMessage(messages.passwordSectionLabel)
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              WalletCreateDialog_scss_1.default.passwordSectionDescription,
          },
          intl.formatMessage(messages.passwordSectionDescription)
        ),
        react_1.default.createElement(
          'div',
          {
            className: WalletCreateDialog_scss_1.default.spendingPasswordFields,
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
                className: WalletCreateDialog_scss_1.default.infoIcon,
              })
            )
          ),
          react_1.default.createElement(
            'div',
            {
              className:
                WalletCreateDialog_scss_1.default.spendingPasswordField,
            },
            react_1.default.createElement(PasswordInput_1.PasswordInput, {
              className: 'repeatedPassword',
              onKeyPress: this.handleSubmitOnEnter,
              ...repeatedPasswordField.bind(),
              repeatPassword: spendingPasswordField.value,
              isPasswordRepeat: true,
            })
          ),
          react_1.default.createElement(
            'p',
            {
              className: WalletCreateDialog_scss_1.default.passwordInstructions,
            },
            react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
              ...global_messages_1.default.passwordInstructions,
            })
          )
        )
      )
    );
  }
};
WalletCreateDialog = __decorate([mobx_react_1.observer], WalletCreateDialog);
exports.default = WalletCreateDialog;
//# sourceMappingURL=WalletCreateDialog.js.map
