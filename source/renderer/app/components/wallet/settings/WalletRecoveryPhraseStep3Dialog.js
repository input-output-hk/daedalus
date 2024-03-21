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
exports.messages = void 0;
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRecoveryPhraseStepDial... Remove this comment to see the full error message
const WalletRecoveryPhraseStepDialogs_scss_1 = __importDefault(
  require('./WalletRecoveryPhraseStepDialogs.scss')
);
exports.messages = (0, react_intl_1.defineMessages)({
  recoveryPhraseStep3Title: {
    id: 'wallet.settings.recoveryPhraseStep3Title',
    defaultMessage: '!!!verification successful',
    description: 'Label for the recoveryPhraseStep3Title on wallet settings.',
  },
  recoveryPhraseStep3Paragraph1: {
    id: 'wallet.settings.recoveryPhraseStep3Paragraph1',
    defaultMessage:
      '!!!You have verified the recovery phrase for this wallet. You can use it at any time to recover the funds in this wallet on another computer, even if using a different version of Daedalus.',
    description:
      'Label for the recoveryPhraseStep3Paragraph1 on wallet settings.',
  },
  recoveryPhraseStep3Paragraph2: {
    id: 'wallet.settings.recoveryPhraseStep3Paragraph2',
    defaultMessage:
      '!!!Please make sure to return your wallet recovery phrase to a secure place for safekeeping. Anyone with access to your wallet recovery phrase can take control of your funds.',
    description:
      'Label for the recoveryPhraseStep3Paragraph2 on wallet settings.',
  },
  recoveryPhraseStep3Button: {
    id: 'wallet.settings.recoveryPhraseStep3Button',
    defaultMessage: '!!!Finish',
    description: 'Label for the recoveryPhraseStep3Button on wallet settings.',
  },
});
let WalletRecoveryPhraseStep3Dialog = class WalletRecoveryPhraseStep3Dialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    safetyAgreement: false,
  };
  onToggleSafetyAgreement = (checked) => {
    this.setState({
      safetyAgreement: checked,
    });
  };
  render() {
    const { intl } = this.context;
    const { onClose, walletName } = this.props;
    const { safetyAgreement } = this.state;
    const actions = [
      {
        label: intl.formatMessage(exports.messages.recoveryPhraseStep3Button),
        primary: true,
        onClick: onClose,
        disabled: !safetyAgreement,
      },
    ];
    const dialogStyles = (0, classnames_1.default)([
      WalletRecoveryPhraseStepDialogs_scss_1.default.dialog,
      'verification-successful',
    ]);
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogStyles,
        title: intl.formatMessage(exports.messages.recoveryPhraseStep3Title),
        subtitle: walletName,
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onClose,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
      },
      react_1.default.createElement(
        'p',
        null,
        intl.formatMessage(exports.messages.recoveryPhraseStep3Paragraph1)
      ),
      react_1.default.createElement(
        'div',
        {
          className:
            WalletRecoveryPhraseStepDialogs_scss_1.default.checkboxContainer,
        },
        react_1.default.createElement(Checkbox_1.Checkbox, {
          onChange: this.onToggleSafetyAgreement,
          checked: safetyAgreement,
          skin: CheckboxSkin_1.CheckboxSkin,
          className: WalletRecoveryPhraseStepDialogs_scss_1.default.checkbox,
          label: intl.formatMessage(
            exports.messages.recoveryPhraseStep3Paragraph2
          ),
        })
      )
    );
  }
};
WalletRecoveryPhraseStep3Dialog = __decorate(
  [mobx_react_1.observer],
  WalletRecoveryPhraseStep3Dialog
);
exports.default = WalletRecoveryPhraseStep3Dialog;
//# sourceMappingURL=WalletRecoveryPhraseStep3Dialog.js.map
