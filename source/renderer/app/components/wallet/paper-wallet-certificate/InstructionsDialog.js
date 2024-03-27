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
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const network_1 = require('../../../utils/network');
const InstructionsDialog_scss_1 = __importDefault(
  require('./InstructionsDialog.scss')
);
const ReactToolboxMobxForm_1 = require('../../../utils/ReactToolboxMobxForm');
const cryptoConfig_1 = require('../../../config/cryptoConfig');
const environment_types_1 = require('../../../../../common/types/environment.types');
const messages = (0, react_intl_1.defineMessages)({
  headline: {
    id: 'paper.wallet.create.certificate.instructions.dialog.headline',
    defaultMessage: '!!!Create a paper wallet certificate',
    description:
      'Headline for the "Paper wallet create certificate instructions dialog".',
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.instructions.dialog.subtitle',
    defaultMessage:
      '!!!Create a paper wallet certificate to store funds offline.',
    description:
      'Subtitle for the "Paper wallet create certificate instructions dialog".',
  },
  subtitle2: {
    id: 'paper.wallet.create.certificate.instructions.dialog.subtitle2',
    defaultMessage:
      '!!!The paper wallet certificate will not be associated with any of your existing wallets. A new, empty wallet will be created.',
    description:
      'subtitle2 for the "Paper wallet create certificate instructions dialog".',
  },
  instructionsListLabel: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.label',
    defaultMessage: '!!!Instructions',
    description:
      'Instructions list label for the "Paper wallet create certificate instructions dialog".',
  },
  instructionsListDefinition1: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition1',
    defaultMessage: `!!!Your printed certificate will include your paper wallet recovery phrase
      of {paperWalletRecoveryPhraseWordCount} words. Note that your paper wallet recovery phrase is
      different to the {walletRecoveryPhraseWordCount}-word recovery phrases used to restore your
      regular Daedalus wallet.`,
    description: 'Wallet certificate create instructions dialog definition 1.',
  },
  instructionsListDefinition2: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition2',
    defaultMessage: `!!!For security reasons, the last {paperWalletWrittenWordsCount} words of your
      paper wallet recovery phrase will not be printed on the paper wallet certificate itself. You
      will need to write them on your certificate by hand in a moment.`,
    description: 'Wallet certificate create instructions dialog definition 2.',
  },
  instructionsListDefinition3: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition3',
    defaultMessage:
      '!!!Use the address on your certificate to send funds to your paper wallet.',
    description: 'Wallet certificate create instructions dialog definition 3.',
  },
  instructionsListDefinition4: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition4',
    defaultMessage: `!!!Your paper wallet will be offline so will not be held in Daedalus.
      To check the balance of the wallet, input the address on the certificate into`,
    description: 'Wallet certificate create instructions dialog definition 4.',
  },
  instructionsListDefinition5: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.instructionsList.definition5',
    defaultMessage:
      '!!!Store your certificate containing your paper wallet recovery phrase in a safe place.',
    description: 'Wallet certificate create instructions dialog definition 5.',
  },
  printingInstructions: {
    id:
      'paper.wallet.create.certificate.instructions.dialog.printingInstructions',
    defaultMessage: `!!!When you click “Save PDF file for printing” you will be prompted
      to choose a location on your computer where the PDF file will be saved. After that
      open the saved PDF file and print it.`,
    description:
      'Wallet certificate create instructions dialog - printing instructions.',
  },
  cardanoExplorer: {
    id: 'paper.wallet.create.certificate.instructions.dialog.cardanoExplorer',
    defaultMessage: '!!!Cardano Explorer',
    description:
      'Wallet certificate create instructions dialog "Cardano Explorer" label',
  },
  printButtonLabel: {
    id: 'paper.wallet.create.certificate.instructions.dialog.button.printLabel',
    defaultMessage: '!!!Save PDF file for printing',
    description:
      '"Wallet certificate create instructions dialog" print button label.',
  },
});
let InstructionsDialog = class InstructionsDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    network: environment_types_1.DEVELOPMENT,
  };
  componentDidUpdate(prevProps) {
    if (!prevProps.error && this.props.error) {
      (0, ReactToolboxMobxForm_1.handleFormErrors)(
        '.InstructionsDialog_error',
        {
          focusElement: true,
        }
      );
    }
  }
  render() {
    const { intl } = this.context;
    const {
      onClose,
      onPrint,
      inProgress,
      onOpenExternalLink,
      network,
      error,
    } = this.props;
    const printButtonClasses = (0, classnames_1.default)([
      'printButton',
      inProgress
        ? InstructionsDialog_scss_1.default.submitButtonSpinning
        : null,
    ]);
    const actions = [
      {
        className: printButtonClasses,
        label: intl.formatMessage(messages.printButtonLabel),
        primary: true,
        onClick: onPrint,
      },
    ];
    const openNetworkExplorer = () =>
      onOpenExternalLink((0, network_1.getNetworkExplorerUrl)(network));
    const cardanoExplorerLink = react_1.default.createElement(Link_1.Link, {
      className: InstructionsDialog_scss_1.default.link,
      onClick: openNetworkExplorer,
      label: intl.formatMessage(messages.cardanoExplorer),
      skin: LinkSkin_1.LinkSkin,
    });
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: 'instructionsDialog',
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
        {
          className:
            InstructionsDialog_scss_1.default.instructionsContentWrapper,
        },
        react_1.default.createElement(
          'p',
          { className: InstructionsDialog_scss_1.default.subtitle },
          intl.formatMessage(messages.subtitle)
        ),
        react_1.default.createElement(
          'p',
          { className: InstructionsDialog_scss_1.default.subtitle2 },
          intl.formatMessage(messages.subtitle2)
        ),
        react_1.default.createElement(
          'div',
          { className: InstructionsDialog_scss_1.default.instructionsList },
          react_1.default.createElement(
            'p',
            {
              className:
                InstructionsDialog_scss_1.default.instructionsListLabel,
            },
            intl.formatMessage(messages.instructionsListLabel)
          ),
          react_1.default.createElement(
            'ul',
            null,
            react_1.default.createElement(
              'li',
              null,
              intl.formatMessage(messages.instructionsListDefinition1, {
                paperWalletRecoveryPhraseWordCount:
                  cryptoConfig_1.PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
                walletRecoveryPhraseWordCount:
                  cryptoConfig_1.WALLET_RECOVERY_PHRASE_WORD_COUNT,
              })
            ),
            react_1.default.createElement(
              'li',
              null,
              intl.formatMessage(messages.instructionsListDefinition2, {
                paperWalletWrittenWordsCount:
                  cryptoConfig_1.PAPER_WALLET_WRITTEN_WORDS_COUNT,
              })
            ),
            react_1.default.createElement(
              'li',
              null,
              intl.formatMessage(messages.instructionsListDefinition3)
            ),
            react_1.default.createElement(
              'li',
              null,
              react_1.default.createElement(react_intl_1.FormattedMessage, {
                ...messages.instructionsListDefinition4,
                values: {
                  link: cardanoExplorerLink,
                },
              })
            ),
            react_1.default.createElement(
              'li',
              null,
              intl.formatMessage(messages.instructionsListDefinition5)
            )
          )
        ),
        react_1.default.createElement(
          'p',
          { className: InstructionsDialog_scss_1.default.printingInstructions },
          react_1.default.createElement(
            'strong',
            null,
            intl.formatMessage(messages.printingInstructions)
          )
        ),
        error &&
          react_1.default.createElement(
            'p',
            { className: InstructionsDialog_scss_1.default.error },
            intl.formatMessage(error)
          )
      )
    );
  }
};
InstructionsDialog = __decorate([mobx_react_1.observer], InstructionsDialog);
exports.default = InstructionsDialog;
//# sourceMappingURL=InstructionsDialog.js.map
