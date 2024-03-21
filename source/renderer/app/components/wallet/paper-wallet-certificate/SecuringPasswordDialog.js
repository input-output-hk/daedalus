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
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/paper-w... Remove this comment to see the full error message
const certificate_png_1 = __importDefault(
  require('../../../assets/images/paper-wallet-certificate/certificate.png')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const SecuringPasswordDialog_scss_1 = __importDefault(
  require('./SecuringPasswordDialog.scss')
);
const cryptoConfig_1 = require('../../../config/cryptoConfig');
const messages = (0, react_intl_1.defineMessages)({
  headline: {
    id: 'paper.wallet.create.certificate.securingPassword.dialog.headline',
    defaultMessage: '!!!Complete your certificate',
    description:
      'Headline for the "Paper wallet create certificate securing password dialog".',
  },
  infoLabel1: {
    id: 'paper.wallet.create.certificate.securingPassword.dialog.infoLabel1',
    defaultMessage: `!!!To complete your paper wallet certificate you will need to
      write the remaining {paperWalletWrittenWordsCount} words of your paper wallet recovery
      phrase on your certificate.`,
    description:
      '"Paper wallet create certificate securing password dialog" first info label.',
  },
  infoLabel2: {
    id: 'paper.wallet.create.certificate.securingPassword.dialog.infoLabel2',
    defaultMessage:
      '!!!The password can optionally be written on the certificate or kept securely in other location. Here is the placeholder on the certificate intended for your password.',
    description: 'You may write the remaining words here:',
  },
  securingPasswordConfirmation: {
    id:
      'paper.wallet.create.certificate.securingPassword.dialog.securingPasswordConfirmation',
    defaultMessage:
      '!!!I have written the remaining {paperWalletWrittenWordsCount} words on the certificate.',
    description:
      '"Paper wallet create certificate securing password dialog" secure password confirmation.',
  },
});
let SecuringPasswordDialog = class SecuringPasswordDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    securePasswordConfirmed: false,
  };
  onSecurePasswordConfirmation = () => {
    this.setState((prevState) => ({
      securePasswordConfirmed: !prevState.securePasswordConfirmed,
    }));
  };
  render() {
    const { intl } = this.context;
    const { securePasswordConfirmed } = this.state;
    const { additionalMnemonics, onContinue, onClose } = this.props;
    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(
          global_messages_1.default.dialogButtonContinueLabel
        ),
        primary: true,
        disabled: !securePasswordConfirmed,
        onClick: onContinue,
      },
    ];
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: 'SecuringPasswordDialog',
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
            SecuringPasswordDialog_scss_1.default
              .securingPasswordContentWrapper,
        },
        react_1.default.createElement(
          'div',
          { className: SecuringPasswordDialog_scss_1.default.content },
          react_1.default.createElement(
            'p',
            { className: SecuringPasswordDialog_scss_1.default.infoLabel },
            intl.formatMessage(messages.infoLabel1, {
              paperWalletWrittenWordsCount:
                cryptoConfig_1.PAPER_WALLET_WRITTEN_WORDS_COUNT,
            })
          ),
          react_1.default.createElement(
            'div',
            { className: SecuringPasswordDialog_scss_1.default.recoveryPhrase },
            additionalMnemonics
          ),
          react_1.default.createElement(
            'p',
            { className: SecuringPasswordDialog_scss_1.default.infoLabel },
            intl.formatMessage(messages.infoLabel2)
          ),
          react_1.default.createElement(
            'div',
            {
              className:
                SecuringPasswordDialog_scss_1.default.paperWalletImageWrapper,
            },
            react_1.default.createElement('img', {
              src: certificate_png_1.default,
              role: 'presentation',
            })
          ),
          react_1.default.createElement(Checkbox_1.Checkbox, {
            className:
              SecuringPasswordDialog_scss_1.default
                .securingPasswordConfirmation,
            label: intl.formatMessage(messages.securingPasswordConfirmation, {
              paperWalletWrittenWordsCount:
                cryptoConfig_1.PAPER_WALLET_WRITTEN_WORDS_COUNT,
            }),
            onChange: this.onSecurePasswordConfirmation,
            checked: securePasswordConfirmed,
            skin: CheckboxSkin_1.CheckboxSkin,
          })
        )
      )
    );
  }
};
SecuringPasswordDialog = __decorate(
  [mobx_react_1.observer],
  SecuringPasswordDialog
);
exports.default = SecuringPasswordDialog;
//# sourceMappingURL=SecuringPasswordDialog.js.map
