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
const classnames_1 = __importDefault(require('classnames'));
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const WalletSummaryHeader_scss_1 = __importDefault(
  require('./WalletSummaryHeader.scss')
);
const formatters_1 = require('../../../utils/formatters');
const discreet_mode_1 = require('../../../features/discreet-mode');
const WalletSummaryHeaderRewards_1 = __importDefault(
  require('./WalletSummaryHeaderRewards')
);
const messages = (0, react_intl_1.defineMessages)({
  transactionsLabel: {
    id: 'wallet.summary.header.transactionsLabel',
    defaultMessage: '!!!{total} transactions, {pending} pending',
    description: '"Number of transactions" label on Wallet summary header page',
  },
});
let WalletSummaryHeader = class WalletSummaryHeader extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const {
      wallet,
      reward,
      numberOfPendingTransactions,
      numberOfRecentTransactions,
      numberOfTransactions,
      isLoadingTransactions,
      currency,
    } = this.props;
    const { intl } = this.context;
    const isLoadingAllTransactions =
      numberOfRecentTransactions && !numberOfTransactions;
    const numberOfTransactionsStyles = (0, classnames_1.default)([
      WalletSummaryHeader_scss_1.default.numberOfTransactions,
      isLoadingAllTransactions
        ? WalletSummaryHeader_scss_1.default.isLoadingNumberOfTransactions
        : null,
    ]);
    const walletNameStyles = (0, classnames_1.default)([
      WalletSummaryHeader_scss_1.default.walletName,
    ]);
    const walletAmountStyles = (0, classnames_1.default)([
      WalletSummaryHeader_scss_1.default.walletAmount,
    ]);
    const isRestoreActive = wallet.isRestoring;
    const walletAmount = isRestoreActive
      ? '–'
      : (0, formatters_1.formattedWalletAmount)(wallet.amount, false);
    const totalTransactions = isRestoreActive
      ? '–'
      : numberOfTransactions ?? numberOfRecentTransactions;
    const pendingTransactions = isRestoreActive
      ? '–'
      : numberOfPendingTransactions;
    return react_1.default.createElement(
      'div',
      { className: WalletSummaryHeader_scss_1.default.component },
      react_1.default.createElement(
        BorderedBox_1.default,
        null,
        react_1.default.createElement(
          'div',
          { className: WalletSummaryHeader_scss_1.default.walletContent },
          react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'div',
              { className: walletNameStyles },
              wallet.name
            ),
            react_1.default.createElement(
              'div',
              { className: walletAmountStyles },
              react_1.default.createElement(
                discreet_mode_1.DiscreetValue,
                null,
                walletAmount
              ),
              react_1.default.createElement(
                'span',
                { className: WalletSummaryHeader_scss_1.default.currencyCode },
                intl.formatMessage(global_messages_1.default.adaUnit)
              )
            ),
            !wallet.isLegacy &&
              react_1.default.createElement(
                'div',
                { className: WalletSummaryHeader_scss_1.default.rewards },
                react_1.default.createElement(
                  WalletSummaryHeaderRewards_1.default,
                  {
                    isRestoring: isRestoreActive,
                    total: reward.total,
                    unspent: reward.unspent,
                    walletAmount: wallet.amount,
                  }
                )
              ),
            !isLoadingTransactions &&
              react_1.default.createElement(
                'div',
                {
                  className:
                    WalletSummaryHeader_scss_1.default.transactionsCountWrapper,
                },
                react_1.default.createElement(
                  'div',
                  { className: numberOfTransactionsStyles },
                  react_1.default.createElement(
                    react_intl_1.FormattedHTMLMessage,
                    {
                      ...messages.transactionsLabel,
                      tagName: 'p',
                      values: {
                        total: totalTransactions,
                        pending: pendingTransactions,
                      },
                    }
                  )
                )
              )
          ),
          react_1.default.createElement(
            'div',
            { className: WalletSummaryHeader_scss_1.default.currency },
            currency
          )
        )
      )
    );
  }
};
WalletSummaryHeader = __decorate([mobx_react_1.observer], WalletSummaryHeader);
exports.default = WalletSummaryHeader;
//# sourceMappingURL=WalletSummaryHeader.js.map
