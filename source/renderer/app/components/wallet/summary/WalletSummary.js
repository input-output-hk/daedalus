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
const WalletSummaryHeader_1 = __importDefault(require('./WalletSummaryHeader'));
const WalletSummaryCurrency_1 = __importDefault(
  require('./WalletSummaryCurrency')
);
const WalletTokensList_1 = __importDefault(
  require('../tokens/wallet-tokens-list/WalletTokensList')
);
const numbersConfig_1 = require('../../../config/numbersConfig');
const messages = (0, react_intl_1.defineMessages)({
  tokensListTitle: {
    id: 'wallet.summary.assets.tokensTitle',
    defaultMessage: '!!!Tokens',
    description: 'Tokens title in the wallet summary',
  },
});
let WalletSummary = class WalletSummary extends react_1.Component {
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
      currentLocale,
      currencyIsActive,
      currencyIsFetchingRate,
      currencyLastFetched,
      currencyRate,
      currencySelected,
      onCurrencySettingClick,
      assets,
      onOpenAssetSend,
      onCopyAssetParam,
      onAssetSettings,
      assetSettingsDialogWasOpened,
      isLoadingAssets,
      onExternalLinkClick,
      onViewAllButtonClick,
      onToggleFavorite,
      tokenFavorites,
    } = this.props;
    const { intl } = this.context;
    const { isRestoring } = wallet;
    return react_1.default.createElement(
      react_1.default.Fragment,
      null,
      react_1.default.createElement(WalletSummaryHeader_1.default, {
        wallet: wallet,
        reward: reward,
        numberOfRecentTransactions: numberOfRecentTransactions,
        numberOfTransactions: numberOfTransactions,
        numberOfPendingTransactions: numberOfPendingTransactions,
        isLoadingTransactions: isLoadingTransactions,
        currency:
          currencyIsActive &&
          react_1.default.createElement(WalletSummaryCurrency_1.default, {
            wallet: wallet,
            currencyIsFetchingRate: currencyIsFetchingRate,
            currencyIsActive: currencyIsActive,
            currencySelected: currencySelected,
            currencyRate: currencyRate,
            currencyLastFetched: currencyLastFetched,
            onCurrencySettingClick: onCurrencySettingClick,
          }),
      }),
      !isRestoring &&
        react_1.default.createElement(WalletTokensList_1.default, {
          assets: assets.slice(0, numbersConfig_1.MAX_TOKENS_ON_SUMMARY_PAGE),
          assetSettingsDialogWasOpened: assetSettingsDialogWasOpened,
          currentLocale: currentLocale,
          isLoadingAssets: isLoadingAssets,
          onAssetSettings: onAssetSettings,
          onCopyAssetParam: onCopyAssetParam,
          onExternalLinkClick: onExternalLinkClick,
          onOpenAssetSend: onOpenAssetSend,
          onToggleFavorite: onToggleFavorite,
          onViewAllButtonClick: onViewAllButtonClick,
          title: intl.formatMessage(messages.tokensListTitle),
          tokenFavorites: tokenFavorites,
          wallet: wallet,
        })
    );
  }
};
WalletSummary = __decorate([mobx_react_1.observer], WalletSummary);
exports.default = WalletSummary;
//# sourceMappingURL=WalletSummary.js.map
