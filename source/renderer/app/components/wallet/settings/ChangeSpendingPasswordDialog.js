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
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const validations_1 = require('../../../utils/validations');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const PasswordInput_1 = require('../../widgets/forms/PasswordInput');
const ChangeSpendingPasswordDialog_scss_1 = __importDefault(
  require('./ChangeSpendingPasswordDialog.scss')
);
const timingConfig_1 = require('../../../config/timingConfig');
const form_1 = require('../../../utils/form');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/info-ic... Remove this comment to see the full error message
const info_icon_inline_svg_1 = __importDefault(
  require('../../../assets/images/info-icon.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  dialogTitleSetPassword: {
    id: 'wallet.settings.changePassword.dialog.title.setPassword',
    defaultMessage: '!!!Set a password for {walletName} wallet',
    description:
      'Title for the "Change wallet password" dialog when there is no password set.',
  },
  dialogTitleChangePassword: {
    id: 'wallet.settings.changePassword.dialog.title.changePassword',
    defaultMessage: '!!!Change password',
    description:
      'Title for the "Change wallet password" dialog when there is already password set.',
  },
  spendingPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description:
      'Label for the "Spending password" input in the change wallet password dialog.',
  },
  currentPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.currentPasswordLabel',
    defaultMessage: '!!!Current password',
    description:
      'Label for the "Current password" input in the change wallet password dialog.',
  },
  newPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.newPasswordLabel',
    defaultMessage: '!!!New password',
    description:
      'Label for the "New password" input in the change wallet password dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.settings.changePassword.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the change wallet password dialog.',
  },
  currentPasswordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.currentPasswordFieldPlaceholder',
    defaultMessage: '!!!Type current password',
    description:
      'Placeholder for the "Current password" inputs in the change wallet password dialog.',
  },
  newPasswordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.newPasswordFieldPlaceholder',
    defaultMessage: '!!!Type new password',
    description:
      'Placeholder for the "New password" inputs in the change wallet password dialog.',
  },
  repeatPasswordFieldPlaceholder: {
    id: 'wallet.settings.changePassword.dialog.repeatPasswordFieldPlaceholder',
    defaultMessage: '!!!Repeat new password',
    description:
      'Placeholder for the "Repeat password" inputs in the change wallet password dialog.',
  },
  passwordTooltip: {
    id: 'wallet.dialog.passwordTooltip',
    defaultMessage:
      'We recommend using a password manager app to manage and store your spending password. Generate a unique password using a password manager and paste it here. Passwords should never be reused.',
    description: 'Tooltip for the password input in the wallet dialog.',
  },
});
let ChangeSpendingPasswordDialog = class ChangeSpendingPasswordDialog extends react_1.Component {
  static defaultProps = {
    currentPasswordValue: '',
    newPasswordValue: '',
    repeatedPasswordValue: '',
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        currentPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.currentPasswordLabel),
          placeholder: this.context.intl.formatMessage(
            messages.currentPasswordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ form }) => {
              if (this.props.isSpendingPasswordSet) {
                const currentPasswordField = form.$('currentPassword');
                return [
                  currentPasswordField.value.length > 0,
                  this.context.intl.formatMessage(
                    global_messages_1.default.invalidSpendingPassword
                  ),
                ];
              }
              return [true];
            },
          ],
        },
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages[
              this.props.isSpendingPasswordSet
                ? 'newPasswordLabel'
                : 'spendingPasswordLabel'
            ]
          ),
          placeholder: this.context.intl.formatMessage(
            messages.newPasswordFieldPlaceholder
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
            messages.repeatPasswordFieldPlaceholder
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
        showErrorsOnClear: true,
      },
    }
  );
  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { currentPassword, spendingPassword } = form.values();
        const passwordData = {
          oldPassword: currentPassword,
          newPassword: spendingPassword,
        };
        this.props.onSave(passwordData);
      },
      onError: () => {},
    });
  };
  handleSubmitOnEnter = form_1.submitOnEnter.bind(this, this.submit);
  handleDataChange = (key, value) => {
    this.props.onDataChange({
      [key]: value,
    });
  };
  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      onCancel,
      isSubmitting,
      error,
      isSpendingPasswordSet,
      walletName,
      currentLocale,
    } = this.props;
    const dialogClasses = (0, classnames_1.default)([
      ChangeSpendingPasswordDialog_scss_1.default.dialog,
      isSpendingPasswordSet ? 'changePasswordDialog' : 'createPasswordDialog',
    ]);
    const confirmButtonClasses = (0, classnames_1.default)([
      'confirmButton',
      isSubmitting
        ? ChangeSpendingPasswordDialog_scss_1.default.isSubmitting
        : null,
    ]);
    const spendingPasswordClasses = (0, classnames_1.default)([
      ChangeSpendingPasswordDialog_scss_1.default.spendingPasswordField,
      currentLocale === 'ja-JP'
        ? ChangeSpendingPasswordDialog_scss_1.default.jpLangTooltipIcon
        : '',
    ]);
    const newPasswordClasses = (0, classnames_1.default)([
      'newPassword',
      ChangeSpendingPasswordDialog_scss_1.default.newPassword,
    ]);
    const currentPasswordField = form.$('currentPassword');
    const newPasswordField = form.$('spendingPassword');
    const repeatedPasswordField = form.$('repeatPassword');
    const canSubmit = !isSubmitting && form.isValid;
    const currentPasswordError =
      // @ts-ignore ts-migrate(2339) FIXME: Property 'code' does not exist on type 'Localizabl... Remove this comment to see the full error message
      canSubmit && error && error.code === 'wrong_encryption_passphrase';
    const actions = [
      {
        className: confirmButtonClasses,
        disabled: !canSubmit,
        label: intl.formatMessage(global_messages_1.default.save),
        onClick: this.submit,
        primary: true,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(
          messages[
            !isSpendingPasswordSet
              ? 'dialogTitleSetPassword'
              : 'dialogTitleChangePassword'
          ],
          {
            walletName,
          }
        ),
        subtitle: walletName,
        actions: actions,
        closeOnOverlayClick: true,
        onClose: !isSubmitting ? onCancel : () => {},
        className: dialogClasses,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onCancel }
        ),
      },
      react_1.default.createElement(
        'div',
        {
          className:
            ChangeSpendingPasswordDialog_scss_1.default.spendingPasswordFields,
        },
        isSpendingPasswordSet &&
          react_1.default.createElement(
            'div',
            { className: spendingPasswordClasses },
            react_1.default.createElement(Input_1.Input, {
              className:
                ChangeSpendingPasswordDialog_scss_1.default.currentPassword,
              error: currentPasswordField.error || currentPasswordError,
              ...currentPasswordField.bind(),
              onKeyPress: this.handleSubmitOnEnter,
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
                className: ChangeSpendingPasswordDialog_scss_1.default.infoIcon,
              })
            )
          ),
        react_1.default.createElement(
          'div',
          { className: newPasswordClasses },
          react_1.default.createElement(
            'div',
            { className: spendingPasswordClasses },
            react_1.default.createElement(PasswordInput_1.PasswordInput, {
              ...newPasswordField.bind(),
              onKeyPress: this.handleSubmitOnEnter,
            }),
            !isSpendingPasswordSet &&
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
                  className:
                    ChangeSpendingPasswordDialog_scss_1.default.infoIcon,
                })
              )
          )
        ),
        react_1.default.createElement(
          'div',
          {
            className:
              ChangeSpendingPasswordDialog_scss_1.default.repeatedPassword,
          },
          react_1.default.createElement(PasswordInput_1.PasswordInput, {
            ...repeatedPasswordField.bind(),
            onKeyPress: this.handleSubmitOnEnter,
            repeatPassword: newPasswordField.value,
            isPasswordRepeat: true,
          })
        ),
        react_1.default.createElement(
          'p',
          {
            className:
              ChangeSpendingPasswordDialog_scss_1.default.passwordInstructions,
          },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...global_messages_1.default.passwordInstructions,
          })
        )
      ),
      error
        ? react_1.default.createElement(
            'p',
            { className: ChangeSpendingPasswordDialog_scss_1.default.error },
            intl.formatMessage(error)
          )
        : null
    );
  }
};
ChangeSpendingPasswordDialog = __decorate(
  [mobx_react_1.observer],
  ChangeSpendingPasswordDialog
);
exports.default = ChangeSpendingPasswordDialog;
//# sourceMappingURL=ChangeSpendingPasswordDialog.js.map
