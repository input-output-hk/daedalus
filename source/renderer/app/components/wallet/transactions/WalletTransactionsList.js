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
exports.WalletTransactionsListScrollContext = void 0;
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const react_intl_1 = require('react-intl');
const moment_1 = __importDefault(require('moment'));
const WalletTransactionsList_scss_1 = __importDefault(
  require('./WalletTransactionsList.scss')
);
const Transaction_1 = __importDefault(require('./Transaction'));
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const VirtualTransactionList_1 = require('./render-strategies/VirtualTransactionList');
const SimpleTransactionList_1 = require('./render-strategies/SimpleTransactionList');
const types_1 = require('./types');
const assets_1 = require('../../../utils/assets');
const analytics_1 = require('../../../analytics');
const messages = (0, react_intl_1.defineMessages)({
  today: {
    id: 'wallet.summary.transactionsList.todayLabel',
    defaultMessage: '!!!Today',
    description: 'Label for the "Today" label on the wallet summary page.',
  },
  yesterday: {
    id: 'wallet.summary.transactionsList.yesterdayLabel',
    defaultMessage: '!!!Yesterday',
    description: 'Label for the "Yesterday" label on the wallet summary page.',
  },
  showMoreTransactionsButtonLabel: {
    id: 'wallet.summary.transactionsList.showMoreTransactionsButtonLabel',
    defaultMessage: '!!!Show more transactions',
    description:
      'Label for the "Show more transactions" button on the wallet summary page.',
  },
  syncingTransactionsMessage: {
    id: 'wallet.summary.transactionsList.syncingTransactionsMessage',
    defaultMessage:
      '!!!Your transaction history for this wallet is being synced with the blockchain.',
    description: 'Syncing transactions message on async wallet restore.',
  },
});
exports.WalletTransactionsListScrollContext = react_1.default.createContext({
  setIsScrolling: () => null,
});
const DATE_FORMAT = 'YYYY-MM-DD';
let WalletTransactionsList = class WalletTransactionsList extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    isRenderingAsVirtualList: false,
    showMoreTransactionsButton: false,
    onShowMoreTransactions: () => {},
    onOpenExternalLink: () => {},
  };
  state = {
    isPreloading: true,
  };
  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;
  componentDidMount() {
    this._isMounted = true;
    setTimeout(() => {
      if (this._isMounted)
        this.setState({
          isPreloading: false,
        });
    }, 0);
  }
  componentWillUnmount() {
    this._isMounted = false;
  }
  expandedTransactionIds = new Map();
  transactionsShowingMetadata = new Map();
  virtualList;
  simpleList;
  loadingSpinner;
  groupTransactionsByDay(transactions) {
    const groups = [];
    for (const transaction of transactions) {
      const date = (0, moment_1.default)(transaction.date);
      let group = groups.find(
        (g) => g.date.format(DATE_FORMAT) === date.format(DATE_FORMAT)
      );
      if (!group) {
        group = new types_1.TransactionsGroup({
          date,
          transactions: [],
        });
        groups.push(group);
      }
      group.transactions.push(transaction);
    }
    return groups.sort((a, b) => b.date.valueOf() - a.date.valueOf());
  }
  isSpinnerVisible() {
    const spinner = this.loadingSpinner;
    if (spinner == null || spinner.root == null) return false;
    const spinnerRect = spinner.root.getBoundingClientRect();
    const clientHeight = document.documentElement
      ? document.documentElement.clientHeight
      : 0;
    const windowHeight = window.innerHeight;
    const viewHeight = Math.max(clientHeight, windowHeight);
    return !(spinnerRect.bottom < 0 || spinnerRect.top - viewHeight >= 0);
  }
  localizedDate(date) {
    const { intl } = this.context;
    const { currentDateFormat } = this.props;
    // TODAY
    const today = (0, moment_1.default)().format(DATE_FORMAT);
    if (date === today) return intl.formatMessage(messages.today);
    // YESTERDAY
    const yesterday = (0, moment_1.default)()
      .subtract(1, 'days')
      .format(DATE_FORMAT);
    if (date === yesterday) return intl.formatMessage(messages.yesterday);
    // PAST DATE
    return (0, moment_1.default)(date).format(currentDateFormat);
  }
  isTxExpanded = (tx) => this.expandedTransactionIds.has(tx.id);
  isTxShowingMetadata = (tx) => this.transactionsShowingMetadata.has(tx.id);
  toggleTransactionExpandedState = (tx) => {
    const isExpanded = this.isTxExpanded(tx);
    if (isExpanded) {
      this.expandedTransactionIds.delete(tx.id);
    } else {
      this.expandedTransactionIds.set(tx.id, tx);
    }
    if (this.virtualList) {
      this.virtualList.updateTxRowHeight(tx, !isExpanded, true);
    } else if (this.simpleList) {
      this.simpleList.forceUpdate();
    }
  };
  /**
   * Update the height of the transaction when metadata is shown
   * @param tx
   */
  onShowMetadata = (tx) => {
    this.transactionsShowingMetadata.set(tx.id, tx);
    if (this.virtualList) {
      this.virtualList.updateTxRowHeight(tx, true, true);
    } else if (this.simpleList) {
      this.simpleList.forceUpdate();
    }
  };
  onShowMoreTransactions = (walletId) => {
    if (this.props.onShowMoreTransactions) {
      this.props.onShowMoreTransactions(walletId);
      this.props.analyticsTracker.sendEvent(
        analytics_1.EventCategories.WALLETS,
        'Clicked Show More Transactions button'
      );
    }
  };
  getExpandedTransactions = () => this.expandedTransactionIds;
  renderGroup = (data) =>
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Moment' is not assignable to par... Remove this comment to see the full error message
    react_1.default.createElement(
      'div',
      { className: WalletTransactionsList_scss_1.default.groupDate },
      this.localizedDate(data.date)
    );
  renderTransaction = (data) => {
    const {
      deletePendingTransaction,
      formattedWalletAmount,
      isRestoreActive,
      onOpenExternalLink,
      getUrlByType,
      walletId,
      isDeletingTransaction,
      currentTimeFormat,
      hasAssetsEnabled,
      getAsset,
      isInternalAddress,
      onCopyAssetParam,
    } = this.props;
    const { isFirstInGroup, isLastInGroup, tx } = data;
    const txClasses = (0, classnames_1.default)([
      WalletTransactionsList_scss_1.default.transaction,
      isFirstInGroup
        ? WalletTransactionsList_scss_1.default.firstInGroup
        : null,
      isLastInGroup ? WalletTransactionsList_scss_1.default.lastInGroup : null,
    ]);
    const txTokens = tx.assets;
    const assetTokens = (0, assets_1.getNonZeroAssetTokens)(txTokens, getAsset);
    const totalRawAssets = tx.assets.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = tx.assets.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;
    return react_1.default.createElement(
      'div',
      { id: `tx-${tx.id}`, className: txClasses },
      react_1.default.createElement(Transaction_1.default, {
        data: tx,
        deletePendingTransaction: deletePendingTransaction,
        formattedWalletAmount: formattedWalletAmount,
        isExpanded: this.isTxExpanded(tx),
        isShowingMetadata: this.isTxShowingMetadata(tx),
        isLastInList: isLastInGroup,
        isRestoreActive: isRestoreActive,
        onDetailsToggled: () => this.toggleTransactionExpandedState(tx),
        onOpenExternalLink: onOpenExternalLink,
        onShowMetadata: () => this.onShowMetadata(tx),
        getUrlByType: getUrlByType,
        state: tx.state,
        walletId: walletId,
        isDeletingTransaction: isDeletingTransaction,
        currentTimeFormat: currentTimeFormat,
        assetTokens: assetTokens,
        hasAssetsEnabled: hasAssetsEnabled,
        isInternalAddress: isInternalAddress,
        isLoadingAssets: isLoadingAssets,
        onCopyAssetParam: onCopyAssetParam,
      })
    );
  };
  renderItem = (row) => {
    if (row instanceof types_1.TransactionsGroup) {
      return this.renderGroup(row);
    }
    if (row instanceof types_1.TransactionInfo) {
      return this.renderTransaction(row);
    }
    return null;
  };
  render() {
    const { intl } = this.context;
    const { isPreloading } = this.state;
    const {
      hasMoreToLoad,
      isLoadingTransactions,
      isRenderingAsVirtualList,
      isRestoreActive,
      showMoreTransactionsButton,
      transactions,
      walletId,
    } = this.props;
    const transactionsGroups = this.groupTransactionsByDay(transactions);
    const loadingSpinner =
      (isLoadingTransactions || hasMoreToLoad) && !isRestoreActive
        ? react_1.default.createElement(LoadingSpinner_1.default, {
            ref: (component) => {
              this.loadingSpinner = component;
            },
          })
        : null;
    const syncingTransactionsSpinner = isRestoreActive
      ? react_1.default.createElement(
          'div',
          {
            className:
              WalletTransactionsList_scss_1.default.syncingTransactionsWrapper,
          },
          react_1.default.createElement(LoadingSpinner_1.default, {
            big: true,
          }),
          react_1.default.createElement(
            'p',
            {
              className:
                WalletTransactionsList_scss_1.default.syncingTransactionsText,
            },
            intl.formatMessage(messages.syncingTransactionsMessage)
          )
        )
      : null;
    const buttonClasses = (0, classnames_1.default)([
      'primary',
      WalletTransactionsList_scss_1.default.showMoreTransactionsButton,
    ]);
    // Generate flat list with dates in-between
    const rows = [];
    transactionsGroups.forEach((group) => {
      // First push the group into the list
      rows.push(group);
      // Followed by all transactions the tx in the group
      group.transactions.forEach((transaction, transactionIndex) => {
        const isFirstInGroup = transactionIndex === 0;
        const isLastInGroup =
          group.transactions.length === transactionIndex + 1;
        rows.push(
          new types_1.TransactionInfo({
            tx: transaction,
            isLastInGroup,
            isFirstInGroup,
          })
        );
      });
    });
    const showMoreTxButton = react_1.default.createElement(Button_1.Button, {
      className: buttonClasses,
      label: intl.formatMessage(messages.showMoreTransactionsButtonLabel),
      onClick: this.onShowMoreTransactions.bind(this, walletId),
      skin: ButtonSkin_1.ButtonSkin,
    });
    if (isPreloading)
      return react_1.default.createElement(
        'div',
        {
          className:
            WalletTransactionsList_scss_1.default.preloadingBlockWrapper,
        },
        react_1.default.createElement(LoadingSpinner_1.default, { big: true })
      );
    return react_1.default.createElement(
      'div',
      { className: WalletTransactionsList_scss_1.default.component },
      syncingTransactionsSpinner,
      isRenderingAsVirtualList
        ? react_1.default.createElement(
            VirtualTransactionList_1.VirtualTransactionList,
            {
              getExpandedTransactions: this.getExpandedTransactions,
              ref: (list) => {
                this.virtualList = list;
              },
              renderRow: this.renderItem,
              rows: rows,
              isLoadingSpinnerShown: loadingSpinner !== null,
              isSyncingSpinnerShown: isRestoreActive,
            }
          )
        : react_1.default.createElement(
            SimpleTransactionList_1.SimpleTransactionList,
            {
              ref: (list) => {
                this.simpleList = list;
              },
              renderRow: this.renderItem,
              rows: rows,
            }
          ),
      showMoreTransactionsButton ? showMoreTxButton : null,
      loadingSpinner
    );
  }
};
WalletTransactionsList = __decorate(
  [mobx_react_1.observer],
  WalletTransactionsList
);
exports.default = WalletTransactionsList;
//# sourceMappingURL=WalletTransactionsList.js.map
