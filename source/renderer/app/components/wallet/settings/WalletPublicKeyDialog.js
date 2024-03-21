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
// @ts-nocheck
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const Input_1 = require('@react-polymorph/components/Input');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const PublicKeyDialog_scss_1 = __importDefault(
  require('./PublicKeyDialog.scss')
);
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const form_1 = require('../../../utils/form');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const timingConfig_1 = require('../../../config/timingConfig');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.settings.walletPublicKeyDialog.title',
    defaultMessage: '!!!Reveal wallet public key',
    description: 'Title "Choose a stake pool" on the reveal Wallet Id dialog.',
  },
  description: {
    id: 'wallet.settings.walletPublicKeyDialog.description',
    defaultMessage:
      '!!!Please enter your spending password to reveal your wallet’s public key.',
    description: 'Description on the reveal Wallet Id dialog.',
  },
  buttonLabel: {
    id: 'wallet.settings.walletPublicKeyDialog.button',
    defaultMessage: '!!!Reveal wallet public key',
    description: 'Description on the reveal Wallet Id dialog.',
  },
});
let WalletPublicKeyDialog = class WalletPublicKeyDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  componentDidUpdate() {
    const { hasReceivedWalletPublicKey, onClose } = this.props;
    if (hasReceivedWalletPublicKey) {
      onClose();
    }
  }
  form = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            global_messages_1.default.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            global_messages_1.default.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (field.value === '') {
                return [
                  false,
                  this.context.intl.formatMessage(
                    global_messages_1.default.fieldIsRequired
                  ),
                ];
              }
              return [true];
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
        const { spendingPassword } = form.values();
        const { onRevealPublicKey } = this.props;
        onRevealPublicKey({
          spendingPassword,
        });
      },
    });
  };
  handleSubmitOnEnter = form_1.submitOnEnter.bind(this, this.submit);
  render() {
    const { intl } = this.context;
    const { onClose, error, walletName } = this.props;
    const { form } = this;
    const spendingPasswordField = form.$('spendingPassword');
    const actions = [
      {
        label: intl.formatMessage(messages.buttonLabel),
        onClick: this.submit,
        primary: true,
        disabled: !this.form.isValid,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        title: intl.formatMessage(messages.title),
        subtitle: walletName,
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          { onClose: onClose }
        ),
      },
      react_1.default.createElement(
        'div',
        { className: PublicKeyDialog_scss_1.default.description },
        intl.formatMessage(messages.description)
      ),
      react_1.default.createElement(Input_1.Input, {
        ...spendingPasswordField.bind(),
        error: spendingPasswordField.error,
        onKeyPress: this.handleSubmitOnEnter,
        autoFocus: true,
      }),
      error &&
        react_1.default.createElement(
          'p',
          { className: PublicKeyDialog_scss_1.default.error },
          intl.formatMessage(error)
        )
    );
  }
};
WalletPublicKeyDialog = __decorate(
  [mobx_react_1.observer],
  WalletPublicKeyDialog
);
exports.default = WalletPublicKeyDialog;
//# sourceMappingURL=WalletPublicKeyDialog.js.map
