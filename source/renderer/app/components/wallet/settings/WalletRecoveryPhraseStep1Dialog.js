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
const react_intl_1 = require('react-intl');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const WalletRecoveryPhraseStepDialogs_scss_1 = __importDefault(
  require('./WalletRecoveryPhraseStepDialogs.scss')
);
exports.messages = (0, react_intl_1.defineMessages)({
  recoveryPhraseStep1Title: {
    id: 'wallet.settings.recoveryPhraseStep1Title',
    defaultMessage: '!!!Wallet recovery phrase verification',
    description: 'Label for the recoveryPhraseStep1Title on wallet settings.',
  },
  recoveryPhraseStep1Paragraph1: {
    id: 'wallet.settings.recoveryPhraseStep1Paragraph1',
    defaultMessage:
      '!!!To verify that you have the correct recovery phrase for this wallet, you can enter it on the following screen.',
    description:
      'Label for the recoveryPhraseStep1Paragraph1 on wallet settings.',
  },
  recoveryPhraseStep1Paragraph2: {
    id: 'wallet.settings.recoveryPhraseStep1Paragraph2',
    defaultMessage:
      '!!!Are you being watched? Please make sure that nobody can see your screen while you are entering your wallet recovery phrase.',
    description:
      'Label for the recoveryPhraseStep1Paragraph2 on wallet settings.',
  },
  recoveryPhraseStep1Button: {
    id: 'wallet.settings.recoveryPhraseStep1Button',
    defaultMessage: '!!!Continue',
    description: 'Label for the recoveryPhraseStep1Button on wallet settings.',
  },
});
let WalletRecoveryPhraseStep1Dialog = class WalletRecoveryPhraseStep1Dialog extends react_1.Component {
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
    const { onContinue, onClose, walletName } = this.props;
    const { safetyAgreement } = this.state;
    const actions = [
      {
        label: intl.formatMessage(exports.messages.recoveryPhraseStep1Button),
        primary: true,
        onClick: onContinue,
        disabled: !safetyAgreement,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: WalletRecoveryPhraseStepDialogs_scss_1.default.dialog,
        title: intl.formatMessage(exports.messages.recoveryPhraseStep1Title),
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
        intl.formatMessage(exports.messages.recoveryPhraseStep1Paragraph1)
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
            exports.messages.recoveryPhraseStep1Paragraph2
          ),
        })
      )
    );
  }
};
WalletRecoveryPhraseStep1Dialog = __decorate(
  [mobx_react_1.observer],
  WalletRecoveryPhraseStep1Dialog
);
exports.default = WalletRecoveryPhraseStep1Dialog;
//# sourceMappingURL=WalletRecoveryPhraseStep1Dialog.js.map
