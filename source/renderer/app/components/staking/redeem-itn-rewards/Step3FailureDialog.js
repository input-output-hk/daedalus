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
const Step3FailureDialog_scss_1 = __importDefault(
  require('./Step3FailureDialog.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/sad-wal... Remove this comment to see the full error message
const sad_wallet_inline_svg_1 = __importDefault(
  require('../../../assets/images/sad-wallet.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross-thin.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  description1NoRewards: {
    id: 'staking.redeemItnRewards.step3.failure.description1NoRewards',
    defaultMessage:
      '!!!No rewards were found in your Incentivized Testnet Rewards wallet. Please make sure that you have entered the correct wallet recovery phrase.',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  description2InvalidWallet: {
    id: 'staking.redeemItnRewards.step3.failure.description2InvalidWallet',
    defaultMessage:
      '!!!Rewards from the wallet corresponding to the recovery phrase you have provided have already been redeemed.',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  description3Generic: {
    id: 'staking.redeemItnRewards.step3.failure.description3Generic',
    defaultMessage:
      '!!!No rewards were found in your Incentivized Testnet Rewards wallet. Please make sure that you have entered the correct wallet recovery phrase and that rewards have not already been redeemed.',
    description: 'description for Redeem Incentivized Testnet - Step 3',
  },
  backButtonLabel: {
    id: 'staking.redeemItnRewards.step3.failure.backButtonLabel',
    defaultMessage: '!!!Back',
    description: 'backButtonLabel for Redeem Incentivized Testnet - Step 3',
  },
  closeWindowLinkLabel: {
    id: 'staking.redeemItnRewards.step3.failure.closeWindowLinkLabel',
    defaultMessage: '!!!Close window',
    description:
      'closeWindowLinkLabel for Redeem Incentivized Testnet - Step 3',
  },
});
let Step3FailureDialog = class Step3FailureDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { onClose, onBack } = this.props;
    const actions = {
      direction: 'column',
      items: [
        {
          primary: true,
          label: intl.formatMessage(messages.backButtonLabel),
          onClick: onBack,
        },
        {
          onClick: onClose,
          label: intl.formatMessage(messages.closeWindowLinkLabel),
          isLink: true,
          hasIconAfter: false,
        },
      ],
    };
    const description = messages.description1NoRewards;
    const closeButton = react_1.default.createElement(
      DialogCloseButton_1.default,
      {
        icon: close_cross_thin_inline_svg_1.default,
        className: Step3FailureDialog_scss_1.default.closeButton,
        onClose: onClose,
      }
    );
    return react_1.default.createElement(
      Dialog_1.default,
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      {
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        actions: actions,
        onClose: onClose,
        closeButton: closeButton,
        closeOnOverlayClick: false,
        fullSize: true,
      },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: sad_wallet_inline_svg_1.default,
        className: Step3FailureDialog_scss_1.default.sadWalletImage,
      }),
      react_1.default.createElement(
        'div',
        { className: Step3FailureDialog_scss_1.default.description },
        intl.formatMessage(description)
      )
    );
  }
};
Step3FailureDialog = __decorate([mobx_react_1.observer], Step3FailureDialog);
exports.default = Step3FailureDialog;
//# sourceMappingURL=Step3FailureDialog.js.map
