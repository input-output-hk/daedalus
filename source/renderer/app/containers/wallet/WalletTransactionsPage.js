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
const WalletTransactions_1 = __importDefault(
  require('../../components/wallet/transactions/WalletTransactions')
);
const network_1 = require('../../utils/network');
const walletsConfig_1 = require('../../config/walletsConfig');
let WalletTransactionsPage = class WalletTransactionsPage extends react_1.Component {
  render() {
    const { actions, stores } = this.props;
    const { app, wallets, addresses, profile, assets } = stores;
    const {
      openExternalLink,
      environment: { network },
    } = app;
    const { isInternalAddress } = addresses;
    const activeWallet = wallets.active;
    const {
      allFiltered,
      filterOptions,
      searchRequest,
      totalAvailable,
      deletePendingTransaction,
      deleteTransactionRequest,
      defaultFilterOptions,
      populatedFilterOptions,
    } = this.props.stores.transactions;
    const {
      currentTimeFormat,
      currentDateFormat,
      currentLocale,
      currentNumberFormat,
    } = profile;
    const { searchLimit = 0 } = filterOptions || {};
    const { transactions: transactionActions } = this.props.actions;
    const { filterTransactions, requestCSVFile } = transactionActions;
    const { onCopyAssetParam } = actions.assets;
    const hasAssetsEnabled = walletsConfig_1.WALLET_ASSETS_ENABLED;
    const { getAsset } = assets;
    const getUrlByType = (type, param) =>
      (0, network_1.getNetworkExplorerUrlByType)(
        type,
        param,
        network,
        currentLocale
      );
    const hasMoreToLoad = () =>
      searchLimit !== null &&
      searchLimit !== undefined &&
      totalAvailable > searchLimit;
    return react_1.default.createElement(WalletTransactions_1.default, {
      activeWallet: activeWallet,
      transactions: allFiltered,
      filterOptions: filterOptions || {},
      defaultFilterOptions: defaultFilterOptions,
      populatedFilterOptions: populatedFilterOptions,
      deletePendingTransaction: deletePendingTransaction,
      isLoadingTransactions: searchRequest.isExecutingFirstTime,
      hasMoreToLoad: hasMoreToLoad(),
      onLoadMore: actions.transactions.loadMoreTransactions.trigger,
      isDeletingTransaction: deleteTransactionRequest.isExecuting,
      onOpenExternalLink: openExternalLink,
      getUrlByType: getUrlByType,
      totalAvailable: totalAvailable,
      currentLocale: currentLocale,
      currentTimeFormat: currentTimeFormat,
      currentNumberFormat: currentNumberFormat,
      currentDateFormat: currentDateFormat,
      onFilter: filterTransactions.trigger,
      onRequestCSVFile: requestCSVFile.trigger,
      hasAssetsEnabled: hasAssetsEnabled,
      isInternalAddress: isInternalAddress,
      getAsset: getAsset,
      onCopyAssetParam: onCopyAssetParam.trigger,
    });
  }
};
WalletTransactionsPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  WalletTransactionsPage
);
exports.default = WalletTransactionsPage;
//# sourceMappingURL=WalletTransactionsPage.js.map
