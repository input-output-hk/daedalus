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
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../../widgets/Dialog'));
const Step3SuccessDialog_scss_1 = __importDefault(
  require('./Step3SuccessDialog.scss')
);
const formatters_1 = require('../../../utils/formatters');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/tada-ic... Remove this comment to see the full error message
const tada_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/tada-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross-thin.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'staking.redeemItnRewards.step3.success.title',
    defaultMessage: '!!!Incentivized Testnet rewards redeemed!',
    description: 'title for Redeem Incentivized Testnet - Step 3',
  },
  description: {
    id: 'staking.redeemItnRewards.step3.success.description',
    defaultMessage:
      '!!!You have successfully redeemed <b>{redeemedRewards}</b> to your <b>{walletName}</b> wallet. This transaction incurred <b>{transactionFees}</b> in transaction fees',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  openWalletButtonLabel: {
    id: 'staking.redeemItnRewards.step3.success.openWalletButtonLabel',
    defaultMessage: '!!!Open the wallet',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  downloadPDFButtonLabel: {
    id: 'staking.redeemItnRewards.step3.success.downloadPDFButtonLabel',
    defaultMessage: '!!!Download PDF certificate',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
});
let Step3SuccessDialog = class Step3SuccessDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      wallet,
      transactionFees,
      redeemedRewards,
      onContinue,
      onPDFDownload,
      onClose,
    } = this.props;
    const { name: walletName } = wallet;
    const actions = [
      {
        primary: true,
        label: intl.formatMessage(messages.openWalletButtonLabel),
        onClick: onContinue,
      },
    ];
    if (onPDFDownload)
      actions.push({
        primary: true,
        label: intl.formatMessage(messages.downloadPDFButtonLabel),
        onClick: onPDFDownload,
      });
    const closeButton = react_1.default.createElement(
      DialogCloseButton_1.default,
      {
        icon: close_cross_thin_inline_svg_1.default,
        className: Step3SuccessDialog_scss_1.default.closeButton,
        onClose: onClose,
      }
    );
    return react_1.default.createElement(
      Dialog_1.default,
      {
        onClose: onClose,
        actions: actions,
        closeButton: closeButton,
        closeOnOverlayClick: false,
        fullSize: true,
      },
      react_1.default.createElement(
        'div',
        { className: Step3SuccessDialog_scss_1.default.title },
        intl.formatMessage(messages.title)
      ),
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: tada_ic_inline_svg_1.default,
        className: Step3SuccessDialog_scss_1.default.tadaImage,
      }),
      react_1.default.createElement(
        'div',
        { className: Step3SuccessDialog_scss_1.default.description },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages.description,
          values: {
            walletName,
            transactionFees: (0, formatters_1.formattedWalletAmount)(
              transactionFees
            ),
            redeemedRewards: (0, formatters_1.formattedWalletAmount)(
              redeemedRewards
            ),
          },
        })
      )
    );
  }
};
Step3SuccessDialog = __decorate([mobx_react_1.observer], Step3SuccessDialog);
exports.default = Step3SuccessDialog;
//# sourceMappingURL=Step3SuccessDialog.js.map
