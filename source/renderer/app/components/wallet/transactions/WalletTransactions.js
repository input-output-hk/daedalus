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
const WalletTransactionsList_1 = __importStar(
  require('./WalletTransactionsList')
);
const WalletTransactionsHeader_1 = __importDefault(
  require('./WalletTransactionsHeader')
);
const FilterResultInfo_1 = __importDefault(require('./FilterResultInfo'));
const WalletNoTransactions_1 = __importDefault(
  require('./WalletNoTransactions')
);
const formatters_1 = require('../../../utils/formatters');
const transaction_1 = require('../../../utils/transaction');
const WalletTransactions_scss_1 = __importDefault(
  require('./WalletTransactions.scss')
);
exports.messages = (0, react_intl_1.defineMessages)({
  noTransactions: {
    id: 'wallet.transactions.no.transactions',
    defaultMessage: '!!!No transactions',
    description: 'Message shown when wallet has no transactions yet.',
  },
});
let WalletTransactions = class WalletTransactions extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    isScrolling: false,
  };
  setIsScrolling = (isScrolling) =>
    this.setState({
      isScrolling,
    });
  walletTransactionsListScrollContextValue = {
    setIsScrolling: this.setIsScrolling,
  };
  onFilter = (filterOptions) => {
    this.props.onFilter(filterOptions);
  };
  render() {
    const { intl } = this.context;
    const { isScrolling } = this.state;
    const {
      activeWallet,
      transactions,
      filterOptions,
      deletePendingTransaction,
      onLoadMore,
      hasMoreToLoad,
      isLoadingTransactions,
      onOpenExternalLink,
      getUrlByType,
      isDeletingTransaction,
      totalAvailable,
      currentDateFormat,
      currentTimeFormat,
      currentNumberFormat,
      currentLocale,
      onRequestCSVFile,
      defaultFilterOptions,
      populatedFilterOptions,
      hasAssetsEnabled,
      getAsset,
      isInternalAddress,
      onCopyAssetParam,
    } = this.props;
    // Guard against potential null values
    if (!filterOptions || !activeWallet) return null;
    let walletTransactions = null;
    // const { searchLimit } = filterOptions;
    const numberOfFilterDimensionsApplied = (0,
    transaction_1.getNumberOfFilterDimensionsApplied)(filterOptions);
    const noTransactionsLabel = intl.formatMessage(
      exports.messages.noTransactions
    );
    const isRestoreActive = activeWallet && activeWallet.isRestoring;
    const isFilterDisabled =
      !transactions.length && !numberOfFilterDimensionsApplied;
    if (!transactions.length) {
      walletTransactions = numberOfFilterDimensionsApplied
        ? react_1.default.createElement(FilterResultInfo_1.default, {
            filtered: 0,
            total: totalAvailable,
          })
        : react_1.default.createElement(WalletNoTransactions_1.default, {
            label: noTransactionsLabel,
          });
    } else {
      walletTransactions = react_1.default.createElement(
        WalletTransactionsList_1.default,
        {
          transactions: transactions,
          deletePendingTransaction: deletePendingTransaction,
          isLoadingTransactions: isLoadingTransactions,
          isRestoreActive: isRestoreActive,
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onLoadMore: onLoadMore,
          hasMoreToLoad: hasMoreToLoad,
          walletId: activeWallet.id,
          isDeletingTransaction: isDeletingTransaction,
          formattedWalletAmount: formatters_1.formattedWalletAmount,
          onOpenExternalLink: onOpenExternalLink,
          getUrlByType: getUrlByType,
          currentLocale: currentLocale,
          currentTimeFormat: currentTimeFormat,
          currentDateFormat: currentDateFormat,
          hasAssetsEnabled: hasAssetsEnabled,
          getAsset: getAsset,
          isRenderingAsVirtualList: true,
          isInternalAddress: isInternalAddress,
          onCopyAssetParam: onCopyAssetParam,
        }
      );
    }
    return react_1.default.createElement(
      WalletTransactionsList_1.WalletTransactionsListScrollContext.Provider,
      { value: this.walletTransactionsListScrollContextValue },
      react_1.default.createElement(
        'div',
        { className: WalletTransactions_scss_1.default.component },
        react_1.default.createElement(WalletTransactionsHeader_1.default, {
          numberOfFilterDimensionsApplied: numberOfFilterDimensionsApplied,
          numberOfTransactions: transactions.length,
          onRequestCSVFile: onRequestCSVFile,
          isScrolling: isScrolling,
          isFilterDisabled: isFilterDisabled,
          filterDialogProps: {
            defaultFilterOptions,
            populatedFilterOptions,
            locale: currentLocale,
            dateFormat: currentDateFormat,
            onFilter: this.onFilter,
            numberFormat: currentNumberFormat,
            isDisabled: isFilterDisabled,
          },
        }),
        walletTransactions
      )
    );
  }
};
WalletTransactions = __decorate([mobx_react_1.observer], WalletTransactions);
exports.default = WalletTransactions;
//# sourceMappingURL=WalletTransactions.js.map
