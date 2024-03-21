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
// import { Input } from '@react-polymorph/components/Input';
// import { InputSkin } from '@react-polymorph/skins/simple/InputSkin';
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const FileUploadWidget_1 = __importDefault(
  require('../../widgets/forms/FileUploadWidget')
);
const validations_1 = require('../../../utils/validations');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const WalletFileImportDialog_scss_1 = __importDefault(
  require('./WalletFileImportDialog.scss')
);
const timingConfig_1 = require('../../../config/timingConfig');
const messages = (0, react_intl_1.defineMessages)({
  headline: {
    id: 'wallet.file.import.dialog.headline',
    defaultMessage: '!!!Import Wallet',
    description: 'headline for "Import wallet from file" dialog.',
  },
  walletFileLabel: {
    id: 'wallet.file.import.dialog.walletFileLabel',
    defaultMessage: '!!!Import file',
    description:
      'Label "Import file" on the dialog for importing a wallet from a file.',
  },
  walletFileHint: {
    id: 'wallet.file.import.dialog.walletFileHint',
    defaultMessage: '!!!Drop file here or click to choose',
    description:
      'Hint for the file upload field on the dialog for importing a wallet from a file.',
  },
  walletNameInputLabel: {
    id: 'wallet.file.import.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description:
      'Label for the "wallet name" input in the wallet file import dialog.',
  },
  walletNameInputHint: {
    id: 'wallet.file.import.dialog.wallet.name.input.hint',
    defaultMessage: '!!!e.g: Shopping Wallet',
    description: 'Hint for the "Wallet name" in the wallet file import dialog.',
  },
  submitLabel: {
    id: 'wallet.file.import.dialog.submitLabel',
    defaultMessage: '!!!Import wallet',
    description:
      'Label "Import wallet" submit button on the dialog for importing a wallet from a file.',
  },
  spendingPasswordLabel: {
    id: 'wallet.file.import.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Wallet password',
    description:
      'Label for the "Wallet password" input in the wallet file import dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.file.import.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the wallet file import dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.file.import.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the wallet file import dialog.',
  },
});
let WalletFileImportDialog = class WalletFileImportDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        walletFilePath: {
          label: 'filePath',
          placeholder: 'filePath',
          type: 'hidden',
        },
        walletName: {
          label: this.context.intl.formatMessage(messages.walletNameInputLabel),
          placeholder: this.context.intl.formatMessage(
            messages.walletNameInputHint
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (field.value.length === 0) return [true];
              return [
                (0, validations_1.isValidWalletName)(field.value),
                this.context.intl.formatMessage(
                  global_messages_1.default.invalidWalletName
                ),
              ];
            },
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
            () => {
              // const repeatPasswordField = form.$('repeatPassword');
              // if (repeatPasswordField.value.length > 0) {
              //   repeatPasswordField.validate({ showErrors: true });
              // }
              // return [
              //   isValidSpendingPassword(field.value),
              //   this.context.intl.formatMessage(
              //     globalMessages.invalidSpendingPassword
              //   ),
              // ];
              return [true]; // @API TODO - missing API v2 endpoint and password declaration
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
            () => {
              // const spendingPassword = form.$('spendingPassword').value;
              // if (spendingPassword.length === 0) return [true];
              // return [
              //   isValidRepeatPassword(spendingPassword, field.value),
              //   this.context.intl.formatMessage(
              //     globalMessages.invalidRepeatPassword
              //   ),
              // ];
              return [true]; // @API TODO - missing API v2 endpoint and password declaration
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
    this.form.submit({
      onSuccess: (form) => {
        const { walletFilePath, spendingPassword, walletName } = form.values();
        const walletData = {
          filePath: walletFilePath,
          spendingPassword,
          walletName: walletName.length > 0 ? walletName : null,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {},
    });
  };
  render() {
    const { intl } = this.context;
    const { form } = this;
    const { isSubmitting, error, onClose } = this.props;
    const walletFilePath = form.$('walletFilePath');
    const dialogClasses = (0, classnames_1.default)([
      WalletFileImportDialog_scss_1.default.component,
      'WalletFileImportDialog',
    ]);
    const actions = [
      {
        className: isSubmitting
          ? WalletFileImportDialog_scss_1.default.isSubmitting
          : null,
        label: intl.formatMessage(messages.submitLabel),
        primary: true,
        disabled: isSubmitting || !walletFilePath.value,
        onClick: this.submit,
      },
    ];
    // const walletNameField = form.$('walletName');
    // const spendingPasswordField = form.$('spendingPassword');
    // const repeatedPasswordField = form.$('repeatPassword');
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogClasses,
        title: intl.formatMessage(messages.headline),
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(
        'div',
        { className: WalletFileImportDialog_scss_1.default.fileUpload },
        react_1.default.createElement(FileUploadWidget_1.default, {
          label: intl.formatMessage(messages.walletFileLabel),
          placeholder: intl.formatMessage(messages.walletFileHint),
          acceptedFileTypes: ['*'],
          selectedFile: walletFilePath.value,
          onFileSelected: (filePath) => {
            // "set(value)" is an unbound method and thus must be explicitly called
            walletFilePath.set(filePath);
          },
        })
      ),
      error &&
        react_1.default.createElement(
          'p',
          { className: WalletFileImportDialog_scss_1.default.error },
          intl.formatMessage(error)
        )
    );
  }
};
WalletFileImportDialog = __decorate(
  [mobx_react_1.observer],
  WalletFileImportDialog
);
exports.default = WalletFileImportDialog;
//# sourceMappingURL=WalletFileImportDialog.js.map
