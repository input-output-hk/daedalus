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
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const WalletRestoreDialog_1 = __importDefault(
  require('./widgets/WalletRestoreDialog')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/tada-ic... Remove this comment to see the full error message
const tada_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/tada-ic.inline.svg')
);
const SuccessDialog_scss_1 = __importDefault(require('./SuccessDialog.scss'));
const walletRestoreConfig_1 = require('../../../config/walletRestoreConfig');
const messages = (0, react_intl_1.defineMessages)({
  closeButtonLabel: {
    id: 'wallet.restore.dialog.step.success.dialog.close',
    defaultMessage: '!!!Close',
    description:
      'Label for Close button on the wallet restore "success" step dialog.',
  },
  descriptionLine1: {
    id: 'wallet.restore.dialog.step.success.dialog.description.line1',
    defaultMessage: '!!!Your wallet has been successfully restored.',
    description:
      'Description "line 1" on the wallet restore "success" step dialog.',
  },
  descriptionLine2: {
    id: 'wallet.restore.dialog.step.success.dialog.description.line2',
    defaultMessage:
      '!!!Restored wallets should have all the funds and transaction history of the original wallet. <strong>If your restored wallet does not have the funds and transaction history you were expecting</strong>, please check that you have the correct wallet recovery phrase for the wallet you were intending to restore.',
    description:
      'Description "line 2" on the wallet restore "success" step dialog.',
  },
  descriptionLine3: {
    id: 'wallet.restore.dialog.step.success.dialog.description.line3',
    defaultMessage:
      '!!!<strong>If your restored wallet is empty, but you were expecting it to have funds</strong>, please check that you used the correct wallet recovery phrase during the restoration process.',
    description:
      'Description "line 3" on the wallet restore "success" step dialog.',
  },
});
class SuccessDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { onClose, walletKindDaedalus, walletKindYoroi } = this.props;
    const isDaedalusBalanceWallet =
      walletKindDaedalus ===
        walletRestoreConfig_1.WALLET_DAEDALUS_KINDS.BYRON_12_WORD ||
      walletKindDaedalus ===
        walletRestoreConfig_1.WALLET_DAEDALUS_KINDS.BYRON_27_WORD;
    const isDaedalusRewardsWallet =
      walletKindDaedalus ===
      walletRestoreConfig_1.WALLET_DAEDALUS_KINDS.SHELLEY_15_WORD;
    const isYoroiBalanceWallet =
      walletKindYoroi ===
      walletRestoreConfig_1.WALLET_YOROI_KINDS.BYRON_15_WORD;
    const isYoroiRewardsWallet =
      walletKindYoroi ===
      walletRestoreConfig_1.WALLET_YOROI_KINDS.SHELLEY_15_WORD;
    return react_1.default.createElement(
      WalletRestoreDialog_1.default,
      {
        actions: [
          {
            primary: true,
            label: intl.formatMessage(messages.closeButtonLabel),
            onClick: onClose,
          },
        ],
        onClose: onClose,
      },
      react_1.default.createElement(
        'div',
        { className: SuccessDialog_scss_1.default.content },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: tada_ic_inline_svg_1.default,
          className: SuccessDialog_scss_1.default.tadaImage,
        }),
        react_1.default.createElement(
          'div',
          { className: SuccessDialog_scss_1.default.description1 },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.descriptionLine1,
          })
        ),
        (isDaedalusRewardsWallet || isYoroiRewardsWallet) &&
          react_1.default.createElement(
            'div',
            { className: SuccessDialog_scss_1.default.description2 },
            react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
              ...messages.descriptionLine2,
            })
          ),
        (isDaedalusBalanceWallet || isYoroiBalanceWallet) &&
          react_1.default.createElement(
            'div',
            { className: SuccessDialog_scss_1.default.description3 },
            react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
              ...messages.descriptionLine3,
            })
          )
      )
    );
  }
}
exports.default = SuccessDialog;
//# sourceMappingURL=SuccessDialog.js.map
