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
const walletNavigationConfig_1 = require('../../../config/walletNavigationConfig');
const Navigation_1 = __importDefault(require('../../navigation/Navigation'));
const messages = (0, react_intl_1.defineMessages)({
  summary: {
    id: 'wallet.navigation.summary',
    defaultMessage: '!!!Summary',
    description: 'Label for the "Summary" nav button in the wallet navigation.',
  },
  send: {
    id: 'wallet.navigation.send',
    defaultMessage: '!!!Send',
    description: 'Label for the "Send" nav button in the wallet navigation.',
  },
  receive: {
    id: 'wallet.navigation.receive',
    defaultMessage: '!!!Receive',
    description: 'Label for the "Receive" nav button in the wallet navigation.',
  },
  transactions: {
    id: 'wallet.navigation.transactions',
    defaultMessage: '!!!Transactions',
    description:
      'Label for the "Transactions" nav button in the wallet navigation.',
  },
  tokens: {
    id: 'wallet.navigation.tokens',
    defaultMessage: '!!!Tokens',
    description: 'Label for the "Tokens" nav button in the wallet navigation.',
  },
  settings: {
    id: 'wallet.navigation.settings',
    defaultMessage: '!!!Settings',
    description:
      'Label for the "Settings" nav button in the wallet navigation.',
  },
  utxo: {
    id: 'wallet.navigation.utxo',
    defaultMessage: '!!!Wallet UTXO distribution',
    description:
      'Label for the "Wallet UTXO distribution" nav button in the wallet navigation.',
  },
  more: {
    id: 'wallet.navigation.more',
    defaultMessage: '!!!More',
    description: 'Label for the "More" nav button in the wallet navigation.',
  },
});
let WalletNavigation = class WalletNavigation extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const {
      isActiveNavItem,
      isLegacy,
      onNavItemClick,
      activeItem,
      hasNotification,
    } = this.props;
    const { intl } = this.context;
    // @ts-ignore ts-migrate(2322) FIXME: Type '({ id: string; label: any; type?: undefined;... Remove this comment to see the full error message
    const items = [
      {
        id: walletNavigationConfig_1.WALLET_NAV_IDS.SUMMARY,
        label: intl.formatMessage(messages.summary),
      },
      {
        id: walletNavigationConfig_1.WALLET_NAV_IDS.SEND,
        label: intl.formatMessage(messages.send),
      },
      {
        id: walletNavigationConfig_1.WALLET_NAV_IDS.RECEIVE,
        label: intl.formatMessage(messages.receive),
      },
      {
        id: walletNavigationConfig_1.WALLET_NAV_IDS.TRANSACTIONS,
        label: intl.formatMessage(messages.transactions),
      },
      {
        id: walletNavigationConfig_1.WALLET_NAV_IDS.TOKENS,
        label: intl.formatMessage(messages.tokens),
      },
      {
        id: walletNavigationConfig_1.WALLET_NAV_IDS.SETTINGS,
        type: 'dropdown',
        label: intl.formatMessage(messages.more),
        hasNotification,
        options: [
          {
            label: intl.formatMessage(messages.settings),
            value: 'settings',
            hasNotification,
          },
          {
            label: intl.formatMessage(messages.utxo),
            value: 'utxo',
          },
        ],
      },
    ].filter(
      (item) =>
        !(
          isLegacy &&
          (0, lodash_1.includes)(
            walletNavigationConfig_1.LEGACY_WALLET_EXCLUDED_NAV_ITEMS,
            item.id
          )
        )
    );
    return react_1.default.createElement(Navigation_1.default, {
      activeItem: activeItem,
      isActiveNavItem: isActiveNavItem,
      onNavItemClick: onNavItemClick,
      items: items,
    });
  }
};
WalletNavigation = __decorate([mobx_react_1.observer], WalletNavigation);
exports.default = WalletNavigation;
//# sourceMappingURL=WalletNavigation.js.map
