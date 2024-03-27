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
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const WalletRecoveryPhraseStepDialogs_scss_1 = __importDefault(
  require('./WalletRecoveryPhraseStepDialogs.scss')
);
exports.messages = (0, react_intl_1.defineMessages)({
  recoveryPhraseStep4Title: {
    id: 'wallet.settings.recoveryPhraseStep4Title',
    defaultMessage: '!!!verification failure',
    description: 'Label for the recoveryPhraseStep4Title on wallet settings.',
  },
  recoveryPhraseStep4Paragraph1: {
    id: 'wallet.settings.recoveryPhraseStep4Paragraph1',
    defaultMessage:
      '!!!The wallet recovery phrase you have entered does not match the recovery phrase of this wallet. Make sure you have entered the wallet recovery phrase which was written down during the wallet creation process for this wallet and make sure the words are in the correct order.',
    description:
      'Label for the recoveryPhraseStep4Paragraph1 on wallet settings.',
  },
  recoveryPhraseStep4Paragraph2: {
    id: 'wallet.settings.recoveryPhraseStep4Paragraph2',
    defaultMessage:
      '!!!If you are unable to verify your wallet recovery phrase, you should create a new wallet and move all of the funds from this wallet to the new wallet. If you do this, make sure you keep the wallet recovery phrase for the new wallet safe and secure.',
    description:
      'Label for the recoveryPhraseStep4Paragraph2 on wallet settings.',
  },
  recoveryPhraseStep4Button: {
    id: 'wallet.settings.recoveryPhraseStep4Button',
    defaultMessage: '!!!Verify recovery phrase again',
    description: 'Label for the recoveryPhraseStep4Button on wallet settings.',
  },
  recoveryPhraseStep4SupportTitle: {
    id: 'wallet.settings.recoveryPhraseStep4SupportTitle',
    defaultMessage: '!!!Read support portal article',
    description:
      'Label for the recoveryPhraseStep4SupportTitle on wallet settings.',
  },
  recoveryPhraseStep4SupportUrl: {
    id: 'wallet.settings.recoveryPhraseStep4SupportUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360035341914',
    description:
      'Label for the recoveryPhraseStep4SupportUrl on wallet settings.',
  },
});
let WalletRecoveryPhraseStep4Dialog = class WalletRecoveryPhraseStep4Dialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { onClose, onContinue, openExternalLink, walletName } = this.props;
    const actions = [
      {
        label: intl.formatMessage(exports.messages.recoveryPhraseStep4Button),
        onClick: onContinue,
        className: 'attention',
      },
    ];
    const dialogStyles = (0, classnames_1.default)([
      WalletRecoveryPhraseStepDialogs_scss_1.default.dialog,
      WalletRecoveryPhraseStepDialogs_scss_1.default.dialog4,
      'verification-unsuccessful',
    ]);
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: dialogStyles,
        title: intl.formatMessage(exports.messages.recoveryPhraseStep4Title),
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
        intl.formatMessage(exports.messages.recoveryPhraseStep4Paragraph1)
      ),
      react_1.default.createElement(
        'p',
        null,
        intl.formatMessage(exports.messages.recoveryPhraseStep4Paragraph2)
      ),
      react_1.default.createElement(
        'div',
        {
          className:
            WalletRecoveryPhraseStepDialogs_scss_1.default
              .supportPortalContainer,
        },
        react_1.default.createElement(Link_1.Link, {
          onClick: (event) =>
            openExternalLink(
              intl.formatMessage(
                exports.messages.recoveryPhraseStep4SupportUrl
              ),
              event
            ),
          label: intl.formatMessage(
            exports.messages.recoveryPhraseStep4SupportTitle
          ),
          skin: LinkSkin_1.LinkSkin,
        })
      )
    );
  }
};
WalletRecoveryPhraseStep4Dialog = __decorate(
  [mobx_react_1.observer],
  WalletRecoveryPhraseStep4Dialog
);
exports.default = WalletRecoveryPhraseStep4Dialog;
//# sourceMappingURL=WalletRecoveryPhraseStep4Dialog.js.map
