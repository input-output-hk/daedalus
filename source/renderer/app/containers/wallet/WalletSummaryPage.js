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
const lodash_1 = require('lodash');
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const numbersConfig_1 = require('../../config/numbersConfig');
const AssetSettingsDialog_1 = __importDefault(
  require('../../components/assets/AssetSettingsDialog')
);
const WalletTransactionsList_1 = __importDefault(
  require('../../components/wallet/transactions/WalletTransactionsList')
);
const WalletSummary_1 = __importDefault(
  require('../../components/wallet/summary/WalletSummary')
);
const WalletNoTransactions_1 = __importDefault(
  require('../../components/wallet/transactions/WalletNoTransactions')
);
const VerticalFlexContainer_1 = __importDefault(
  require('../../components/layout/VerticalFlexContainer')
);
const routes_config_1 = require('../../routes-config');
const formatters_1 = require('../../utils/formatters');
const network_1 = require('../../utils/network');
const walletsConfig_1 = require('../../config/walletsConfig');
const assets_1 = require('../../utils/assets');
const withAnalytics_1 = require('../../components/analytics/withAnalytics');
exports.messages = (0, react_intl_1.defineMessages)({
  noTransactions: {
    id: 'wallet.summary.page.no.transactions',
    defaultMessage: '!!!No recent transactions',
    description:
      'Message shown when wallet has no transactions on wallet summary page.',
  },
});
let WalletSummaryPage = class WalletSummaryPage extends react_1.Component {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  handleShowMoreTransaction = (walletId) => {
    this.props.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.WALLETS.PAGE,
      params: {
        id: walletId,
        page: 'transactions',
      },
    });
  };
  handleCurrencySettingsClick = () => {
    this.props.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.SETTINGS.WALLETS,
    });
  };
  handleViewAllButtonClick = (walletId) => {
    this.props.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.WALLETS.PAGE,
      params: {
        id: walletId,
        page: 'tokens',
      },
    });
  };
  openAssetSettingsDialog = ({ asset }) => {
    const { assets, dialogs } = this.props.actions;
    assets.setEditedAsset.trigger({
      asset,
    });
    dialogs.open.trigger({
      dialog: AssetSettingsDialog_1.default,
    });
  };
  get assetSettingsDialogWasOpened() {
    return this.props.stores.uiDialogs.isOpen(AssetSettingsDialog_1.default);
  }
  render() {
    const { intl } = this.context;
    const { stores, actions } = this.props;
    const {
      app,
      wallets,
      addresses,
      transactions,
      profile,
      assets,
      currency,
      staking,
    } = stores;
    const { all, getAsset, favorites } = assets;
    const { isInternalAddress } = addresses;
    const {
      onOpenAssetSend,
      onCopyAssetParam,
      onToggleFavorite,
    } = actions.assets;
    const {
      openExternalLink,
      environment: { network },
    } = app;
    const {
      hasAny,
      totalAvailable,
      recent,
      recentTransactionsRequest,
      deletePendingTransaction,
      deleteTransactionRequest,
      pendingTransactionsCount,
    } = transactions;
    const { active: wallet } = wallets;
    const { isActive, isFetchingRate, lastFetched, rate, selected } = currency;
    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;
    const hasAssetsEnabled = walletsConfig_1.WALLET_ASSETS_ENABLED;
    // Guard against potential null values
    if (!wallet)
      throw new Error('Active wallet required for WalletSummaryPage.');
    let walletTransactions = null;
    const noTransactionsLabel = intl.formatMessage(
      exports.messages.noTransactions
    );
    const walletTokens = wallet.assets.total;
    const assetTokens = (0, assets_1.getAssetTokens)(all, walletTokens).sort(
      (0, assets_1.sortAssets)('token', 'asc')
    );
    const totalRawAssets = wallet.assets.total.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = wallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;
    const onViewAllButtonClick = () => this.handleViewAllButtonClick(wallet.id);
    const getUrlByType = (type, param) =>
      (0, network_1.getNetworkExplorerUrlByType)(
        type,
        param,
        network,
        currentLocale
      );
    if (
      recentTransactionsRequest.isExecutingFirstTime ||
      hasAny ||
      wallet.isRestoring
    ) {
      walletTransactions = react_1.default.createElement(
        WalletTransactionsList_1.default,
        {
          key: `WalletTransactionsList_${wallet.id}`,
          transactions: (0, lodash_1.take)(
            recent,
            numbersConfig_1.MAX_TRANSACTIONS_ON_SUMMARY_PAGE
          ),
          isLoadingTransactions: recentTransactionsRequest.isExecutingFirstTime,
          hasMoreToLoad: false,
          deletePendingTransaction: deletePendingTransaction,
          walletId: wallet.id,
          isDeletingTransaction: deleteTransactionRequest.isExecuting,
          isRestoreActive: wallet.isRestoring,
          formattedWalletAmount: formatters_1.formattedWalletAmount,
          showMoreTransactionsButton:
            recent.length > numbersConfig_1.MAX_TRANSACTIONS_ON_SUMMARY_PAGE,
          onOpenExternalLink: openExternalLink,
          getUrlByType: getUrlByType,
          onShowMoreTransactions: this.handleShowMoreTransaction,
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          totalAvailable: totalAvailable,
          currentTimeFormat: currentTimeFormat,
          currentDateFormat: currentDateFormat,
          isInternalAddress: isInternalAddress,
          hasAssetsEnabled: hasAssetsEnabled,
          getAsset: getAsset,
          onCopyAssetParam: onCopyAssetParam.trigger,
          analyticsTracker: this.props.analyticsTracker,
        }
      );
    } else if (!hasAny) {
      walletTransactions = react_1.default.createElement(
        WalletNoTransactions_1.default,
        { label: noTransactionsLabel }
      );
    }
    return react_1.default.createElement(
      VerticalFlexContainer_1.default,
      null,
      react_1.default.createElement(WalletSummary_1.default, {
        wallet: wallet,
        reward: staking.getRewardForWallet(wallet),
        numberOfRecentTransactions: recent.length,
        numberOfTransactions: totalAvailable,
        numberOfPendingTransactions: pendingTransactionsCount,
        isLoadingTransactions: recentTransactionsRequest.isExecutingFirstTime,
        isLoadingAssets: isLoadingAssets,
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        hasAssetsEnabled: hasAssetsEnabled && hasRawAssets,
        currentLocale: currentLocale,
        currencyIsActive: isActive,
        currencyIsFetchingRate: isFetchingRate,
        currencyLastFetched: lastFetched,
        currencyRate: rate,
        currencySelected: selected,
        onCurrencySettingClick: this.handleCurrencySettingsClick,
        assets: assetTokens,
        assetSettingsDialogWasOpened: this.assetSettingsDialogWasOpened,
        onOpenAssetSend: onOpenAssetSend.trigger,
        onCopyAssetParam: onCopyAssetParam.trigger,
        onAssetSettings: this.openAssetSettingsDialog,
        onExternalLinkClick: app.openExternalLink,
        onViewAllButtonClick: onViewAllButtonClick,
        tokenFavorites: favorites,
        onToggleFavorite: onToggleFavorite.trigger,
      }),
      walletTransactions
    );
  }
};
WalletSummaryPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletSummaryPage
);
exports.default = (0, withAnalytics_1.withAnalytics)(WalletSummaryPage);
//# sourceMappingURL=WalletSummaryPage.js.map
