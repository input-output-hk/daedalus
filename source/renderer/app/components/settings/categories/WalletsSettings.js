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
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const Select_1 = require('@react-polymorph/components/Select');
const Link_1 = require('@react-polymorph/components/Link');
const NormalSwitch_1 = __importDefault(
  require('../../widgets/forms/NormalSwitch')
);
const WalletsSettings_scss_1 = __importDefault(
  require('./WalletsSettings.scss')
);
const currencyConfig_1 = require('../../../config/currencyConfig');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const Separator_1 = require('../../widgets/separator/Separator');
const messages = (0, react_intl_1.defineMessages)({
  currencyTitleLabel: {
    id: 'settings.wallets.currency.titleLabel',
    defaultMessage: '!!!Display ada balances in other currency',
    description:
      'titleLabel for the Currency settings in the Wallets settings page.',
  },
  currencyDescription: {
    id: 'settings.wallets.currency.description',
    defaultMessage:
      '!!!Select a conversion currency for displaying your ada balances.',
    description:
      'currencyDescription for the Currency settings in the Wallets settings page.',
  },
  currencySelectLabel: {
    id: 'settings.wallets.currency.selectLabel',
    defaultMessage: '!!!Select currency',
    description:
      'currencySelectLabel for the Currency settings in the Wallets settings page.',
  },
  currencyDisclaimer: {
    id: 'settings.wallets.currency.disclaimer',
    defaultMessage:
      '!!!Conversion rates are provided by CoinGecko without any warranty. Please use the calculated conversion value only as a reference. Converted balances reflect the current global average price of ada on active cryptocurrency exchanges, as tracked by CoinGecko. Ada conversion is available only to fiat and cryptocurrencies that are supported by CoinGecko, other local currency conversions may not be available.',
    description:
      'currencyDisclaimer for the Currency settings in the Wallets settings page.',
  },
  currencyPoweredByLabel: {
    id: 'settings.wallets.currency.poweredBy.label',
    defaultMessage: '!!!Powered by ',
    description:
      'currencyPoweredByLabel for the Currency settings in the Wallets settings page.',
  },
});
let WalletSettings = class WalletSettings extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      currencySelected,
      currencyList,
      currencyIsActive,
      onSelectCurrency,
      onToggleCurrencyIsActive,
      onOpenExternalLink,
    } = this.props;
    const currencyOptions = (0, lodash_1.map)(
      currencyList,
      ({ code, name }) => ({
        label: `${code.toUpperCase()} - ${name}`,
        value: code,
      })
    );
    return react_1.default.createElement(
      'div',
      { className: WalletsSettings_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: WalletsSettings_scss_1.default.label },
        intl.formatMessage(messages.currencyTitleLabel)
      ),
      react_1.default.createElement(
        'div',
        { className: WalletsSettings_scss_1.default.description },
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(messages.currencyDescription)
        ),
        react_1.default.createElement(NormalSwitch_1.default, {
          checked: currencyIsActive,
          onChange: onToggleCurrencyIsActive,
        })
      ),
      currencyIsActive &&
        react_1.default.createElement(
          'div',
          { className: WalletsSettings_scss_1.default.content },
          react_1.default.createElement(Separator_1.Separator, {
            className: WalletsSettings_scss_1.default.separator,
          }),
          react_1.default.createElement(
            'div',
            { className: WalletsSettings_scss_1.default.currencyPoweredBy },
            intl.formatMessage(messages.currencyPoweredByLabel),
            react_1.default.createElement(Link_1.Link, {
              className: WalletsSettings_scss_1.default.currencyPoweredByLink,
              onClick: () =>
                onOpenExternalLink(currencyConfig_1.currencyConfig.website),
              label: currencyConfig_1.currencyConfig.name,
            })
          ),
          react_1.default.createElement(Select_1.Select, {
            label: intl.formatMessage(messages.currencySelectLabel),
            value: currencySelected ? currencySelected.code : null,
            options: currencyOptions,
            onChange: onSelectCurrency,
            optionHeight: 50,
            noResultsMessage: intl.formatMessage(
              global_messages_1.default.searchNoResultsMessage
            ),
            hasSearch: true,
          }),
          currencyList.length > 0 &&
            currencySelected &&
            react_1.default.createElement(
              'div',
              { className: WalletsSettings_scss_1.default.disclaimer },
              react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
                ...messages.currencyDisclaimer,
                values: {
                  currencyApiName: currencyConfig_1.currencyConfig.name,
                },
              })
            )
        )
    );
  }
};
WalletSettings = __decorate([mobx_react_1.observer], WalletSettings);
exports.default = WalletSettings;
//# sourceMappingURL=WalletsSettings.js.map
