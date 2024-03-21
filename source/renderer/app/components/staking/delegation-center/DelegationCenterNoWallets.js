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
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const DelegationCenterNoWallets_scss_1 = __importDefault(
  require('./DelegationCenterNoWallets.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/attenti... Remove this comment to see the full error message
const attention_big_thin_inline_svg_1 = __importDefault(
  require('../../../assets/images/attention-big-thin.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  headLine: {
    id: 'staking.delegationCenter.noWallets.headLine',
    defaultMessage:
      '!!!The delegation center is not available because you currently do not have any Shelley-compatible wallets.',
    description: '"No wallets" headLine on the Delegation centre Page.',
  },
  instructions: {
    id: 'staking.delegationCenter.noWallets.instructions',
    defaultMessage:
      '!!!Create a new wallet and transfer in a minimum of {minDelegationFunds} ADA (or restore an existing wallet with funds), then return here to delegate your stake.',
    description: '"No wallets" instructions on the Delegation centre Page.',
  },
  createWalletButtonLabel: {
    id: 'staking.delegationCenter.noWallets.createWalletButtonLabel',
    defaultMessage: '!!!Create wallet',
    description:
      'Label for "Create New Wallet" button on the Delegation centre Page.',
  },
});
class DelegationCenterNoWallets extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { onGoToCreateWalletClick, minDelegationFunds } = this.props;
    return react_1.default.createElement(
      'div',
      { className: DelegationCenterNoWallets_scss_1.default.component },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: attention_big_thin_inline_svg_1.default,
        className: DelegationCenterNoWallets_scss_1.default.icon,
      }),
      react_1.default.createElement(
        'h1',
        null,
        intl.formatMessage(messages.headLine)
      ),
      react_1.default.createElement(
        'p',
        null,
        intl.formatMessage(messages.instructions, {
          minDelegationFunds: new bignumber_js_1.default(
            minDelegationFunds
          ).toFormat(0),
        })
      ),
      react_1.default.createElement(Button_1.Button, {
        className: 'primary',
        onClick: onGoToCreateWalletClick,
        label: intl.formatMessage(messages.createWalletButtonLabel),
        skin: ButtonSkin_1.ButtonSkin,
      })
    );
  }
}
exports.default = DelegationCenterNoWallets;
//# sourceMappingURL=DelegationCenterNoWallets.js.map
