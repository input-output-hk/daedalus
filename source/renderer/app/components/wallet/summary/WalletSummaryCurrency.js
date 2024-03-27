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
const moment_1 = __importDefault(require('moment'));
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/currency... Remove this comment to see the full error message
const currency_settings_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/currency-settings-ic.inline.svg')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const WalletSummaryCurrency_scss_1 = __importDefault(
  require('./WalletSummaryCurrency.scss')
);
const formatters_1 = require('../../../utils/formatters');
const discreet_mode_1 = require('../../../features/discreet-mode');
const messages = (0, react_intl_1.defineMessages)({
  currencyTitle: {
    id: 'wallet.summary.currency.title',
    defaultMessage: '!!!Converts as',
    description: '"Currency - title" label on Wallet summary currency page',
  },
  currencyLastFetched: {
    id: 'wallet.summary.currency.lastFetched',
    defaultMessage: '!!!converted {fetchedTimeAgo}',
    description:
      '"Currency - last fetched" label on Wallet summary currency page',
  },
  currencyIsFetchingRate: {
    id: 'wallet.summary.currency.isFetchingRate',
    defaultMessage: '!!!fetching conversion rates',
    description: '"Currency - Fetching" label on Wallet summary currency page',
  },
});
let WalletSummaryCurrency = class WalletSummaryCurrency extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const {
      wallet,
      currencyIsActive,
      currencyIsFetchingRate,
      currencyLastFetched,
      currencyRate,
      currencySelected,
      onCurrencySettingClick,
    } = this.props;
    const { intl } = this.context;
    const isRestoreActive = wallet.isRestoring;
    const hasCurrency =
      currencyIsActive &&
      !!currencySelected &&
      (!!currencyRate || currencyIsFetchingRate);
    const { decimalDigits } = currencySelected || {};
    let currencyWalletAmount;
    if (isRestoreActive || !currencyRate) currencyWalletAmount = '–';
    else if (hasCurrency && currencyRate)
      currencyWalletAmount = (0, formatters_1.formattedWalletCurrencyAmount)(
        wallet.amount,
        currencyRate,
        decimalDigits
      );
    const currencyWalletAmountSymbol =
      currencySelected && currencySelected.code
        ? currencySelected.code.toUpperCase()
        : '';
    const fetchedTimeAgo = (0, moment_1.default)(currencyLastFetched)
      .locale(intl.locale)
      .fromNow();
    const buttonStyles = (0, classnames_1.default)([
      WalletSummaryCurrency_scss_1.default.currencyLastFetched,
      currencyIsFetchingRate
        ? WalletSummaryCurrency_scss_1.default.currencyIsFetchingRate
        : null,
    ]);
    return react_1.default.createElement(
      'div',
      { className: WalletSummaryCurrency_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: WalletSummaryCurrency_scss_1.default.currencyTitle },
        intl.formatMessage(messages.currencyTitle)
      ),
      react_1.default.createElement(
        'div',
        {
          className: WalletSummaryCurrency_scss_1.default.currencyWalletAmount,
        },
        react_1.default.createElement(
          discreet_mode_1.DiscreetValue,
          null,
          currencyWalletAmount
        ),
        react_1.default.createElement(
          'span',
          null,
          ' ',
          currencyWalletAmountSymbol
        )
      ),
      react_1.default.createElement(
        'div',
        { className: WalletSummaryCurrency_scss_1.default.currencyRate },
        '1 ',
        intl.formatMessage(global_messages_1.default.adaUnit),
        ' = ',
        currencyRate || '–',
        ' ',
        currencyWalletAmountSymbol
      ),
      react_1.default.createElement(
        'button',
        { className: buttonStyles, onClick: onCurrencySettingClick },
        react_1.default.createElement(
          'em',
          null,
          currencyIsFetchingRate
            ? intl.formatMessage(messages.currencyIsFetchingRate)
            : intl.formatMessage(messages.currencyLastFetched, {
                fetchedTimeAgo,
              })
        ),
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: currency_settings_ic_inline_svg_1.default,
          className: WalletSummaryCurrency_scss_1.default.currencySettingsIcon,
        })
      )
    );
  }
};
WalletSummaryCurrency = __decorate(
  [mobx_react_1.observer],
  WalletSummaryCurrency
);
exports.default = WalletSummaryCurrency;
//# sourceMappingURL=WalletSummaryCurrency.js.map
