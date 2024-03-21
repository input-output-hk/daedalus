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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/insecur... Remove this comment to see the full error message
const insecure_wallet_inline_svg_1 = __importDefault(
  require('../../../assets/images/insecure-wallet.inline.svg')
);
const SetWalletPassword_scss_1 = __importDefault(
  require('./SetWalletPassword.scss')
);
const ChangeSpendingPasswordDialogContainer_1 = __importDefault(
  require('../../../containers/wallet/dialogs/settings/ChangeSpendingPasswordDialogContainer')
);
const messages = (0, react_intl_1.defineMessages)({
  setPasswordButton: {
    id: 'wallet.settings.setWalletPassword.dialog.setPasswordButton',
    defaultMessage: '!!!Set a password',
    description:
      'Label for the "Set a password" button in the set wallet password dialog.',
  },
  setPasswordMessage: {
    id: 'wallet.settings.setWalletPassword.dialog.setPasswordMessage',
    defaultMessage:
      '!!!To keep your wallet secure and start using it in Daedalus, you need to set a spending password.',
    description:
      'Message for the "Set a password" button in the set wallet password dialog.',
  },
  setPasswordTitle: {
    id: 'wallet.settings.setWalletPassword.dialog.setPasswordTitle',
    defaultMessage: '!!!Your wallet is not protected with a password',
    description:
      'Title for the "Set wallet password" dialog when there is not password set.',
  },
});
let SetWalletPassword = class SetWalletPassword extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { isSetWalletPasswordDialogOpen, onSetWalletPassword } = this.props;
    return react_1.default.createElement(
      react_1.default.Fragment,
      null,
      react_1.default.createElement(
        'div',
        { className: SetWalletPassword_scss_1.default.component },
        react_1.default.createElement(
          'div',
          { className: SetWalletPassword_scss_1.default.setPasswordDialog },
          react_1.default.createElement(
            'div',
            { className: SetWalletPassword_scss_1.default.setPasswordWrapper },
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: insecure_wallet_inline_svg_1.default,
              className: SetWalletPassword_scss_1.default.insecureWalletIcon,
            }),
            react_1.default.createElement(
              'h2',
              { className: SetWalletPassword_scss_1.default.setPasswordTitle },
              intl.formatMessage(messages.setPasswordTitle)
            ),
            react_1.default.createElement(
              'p',
              {
                className: SetWalletPassword_scss_1.default.setPasswordMessage,
              },
              intl.formatMessage(messages.setPasswordMessage)
            ),
            react_1.default.createElement(
              'button',
              {
                className: SetWalletPassword_scss_1.default.setPasswordButton,
                onClick: onSetWalletPassword,
              },
              intl.formatMessage(messages.setPasswordButton)
            )
          )
        )
      ),
      isSetWalletPasswordDialogOpen &&
        react_1.default.createElement(
          ChangeSpendingPasswordDialogContainer_1.default,
          null
        )
    );
  }
};
SetWalletPassword = __decorate([mobx_react_1.observer], SetWalletPassword);
exports.default = SetWalletPassword;
//# sourceMappingURL=SetWalletPassword.js.map
