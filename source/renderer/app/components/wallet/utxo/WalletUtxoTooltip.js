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
const WalletUtxoTooltip_scss_1 = __importDefault(
  require('./WalletUtxoTooltip.scss')
);
const utxoConfig_1 = require('../../../config/utxoConfig');
exports.messages = (0, react_intl_1.defineMessages)({
  tooltipFirst: {
    id: 'wallet.settings.utxos.tooltipFirst',
    defaultMessage:
      '!!!<b>{walletUtxosAmount}</b> UTXOs containing <br /> <b>{walletAmount}</b> ADA',
    description: 'Tooltip for the "Wallet Utxos - first bar" screen.',
  },
  tooltip: {
    id: 'wallet.settings.utxos.tooltip',
    defaultMessage:
      '!!!<b>{walletUtxosAmount}</b> UTXOs containing <br /> <span> between <b>{previousWalletAmount}</b> and </span> <b>{walletAmount}</b> ADA',
    description: 'Tooltip for the "Wallet Utxos" screen.',
  },
  tooltipLast: {
    id: 'wallet.settings.utxos.tooltipLast',
    defaultMessage:
      '!!!<b>{walletUtxosAmount}</b> UTXOs containing <br /> <b>{walletAmount}</b> ADA',
    description: 'Tooltip for the "Wallet Utxos - last bar" screen.',
  },
});
let WalletUtxoTooltip = class WalletUtxoTooltip extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  getPreviousAmount = (walletAmount) => {
    const walletAmountIndex = utxoConfig_1.PRETTY_WALLET_AMOUNTS.findIndex(
      (wa) => wa === walletAmount
    );
    return utxoConfig_1.PRETTY_WALLET_AMOUNTS[walletAmountIndex - 1];
  };
  render() {
    const { label: walletAmount = '', payload } = this.props;
    const { walletUtxosAmount } = payload?.[0]?.payload || {};
    const previousWalletAmount = this.getPreviousAmount(walletAmount);
    let message = exports.messages.tooltip;
    if (!previousWalletAmount) message = exports.messages.tooltipFirst;
    if (walletAmount === '10K+') message = exports.messages.tooltipLast;
    return react_1.default.createElement(
      'div',
      { className: WalletUtxoTooltip_scss_1.default.component },
      react_1.default.createElement(
        'p',
        null,
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...message,
          values: {
            walletUtxosAmount,
            previousWalletAmount,
            walletAmount,
          },
        })
      )
    );
  }
};
WalletUtxoTooltip = __decorate([mobx_react_1.observer], WalletUtxoTooltip);
exports.default = WalletUtxoTooltip;
//# sourceMappingURL=WalletUtxoTooltip.js.map
