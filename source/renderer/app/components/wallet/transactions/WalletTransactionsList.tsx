import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletTransactionsList.scss'... Remove this comment to see the full error message
import styles from './WalletTransactionsList.scss';
import Transaction from './Transaction';
import { WalletTransaction } from '../../../domains/WalletTransaction';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import { VirtualTransactionList } from './render-strategies/VirtualTransactionList';
import { SimpleTransactionList } from './render-strategies/SimpleTransactionList';
import { TransactionInfo, TransactionsGroup } from './types';
import type { Row } from './types';
import { getNonZeroAssetTokens } from '../../../utils/assets';

const messages = defineMessages({
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
export type ScrollContextType = {
  setIsScrolling: (...args: Array<any>) => any;
};
export const WalletTransactionsListScrollContext = React.createContext<
  ScrollContextType
>({
  setIsScrolling: () => null,
});
type Props = {
  deletePendingTransaction: (...args: Array<any>) => any;
  formattedWalletAmount: (...args: Array<any>) => any;
  hasMoreToLoad: boolean;
  isLoadingTransactions: boolean;
  isRestoreActive: boolean;
  isRenderingAsVirtualList: boolean;
  onShowMoreTransactions?: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  getUrlByType: (...args: Array<any>) => any;
  showMoreTransactionsButton?: boolean;
  transactions: Array<WalletTransaction>;
  walletId: string;
  isDeletingTransaction: boolean;
  currentDateFormat: string;
  currentTimeFormat: string;
  hasAssetsEnabled: boolean;
  getAsset: (...args: Array<any>) => any;
  isInternalAddress: (...args: Array<any>) => any;
  onCopyAssetParam: (...args: Array<any>) => any;
};
type State = {
  isPreloading: boolean;
};
const DATE_FORMAT = 'YYYY-MM-DD';

@observer
class WalletTransactionsList extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
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

  expandedTransactionIds: Map<string, WalletTransaction> = new Map();
  transactionsShowingMetadata: Map<string, WalletTransaction> = new Map();
  virtualList: VirtualTransactionList | null | undefined;
  simpleList: SimpleTransactionList | null | undefined;
  loadingSpinner: LoadingSpinner | null | undefined;

  groupTransactionsByDay(
    transactions: Array<WalletTransaction>
  ): Array<TransactionsGroup> {
    const groups: Array<TransactionsGroup> = [];

    for (const transaction of transactions) {
      const date = moment(transaction.date);
      let group = groups.find(
        (g) => g.date.format(DATE_FORMAT) === date.format(DATE_FORMAT)
      );

      if (!group) {
        group = new TransactionsGroup({
          date,
          transactions: [],
        });
        groups.push(group);
      }

      group.transactions.push(transaction);
    }

    return groups.sort(
      (a: TransactionsGroup, b: TransactionsGroup) =>
        b.date.valueOf() - a.date.valueOf()
    );
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

  localizedDate(date: string) {
    const { intl } = this.context;
    const { currentDateFormat } = this.props;
    // TODAY
    const today = moment().format(DATE_FORMAT);
    if (date === today) return intl.formatMessage(messages.today);
    // YESTERDAY
    const yesterday = moment().subtract(1, 'days').format(DATE_FORMAT);
    if (date === yesterday) return intl.formatMessage(messages.yesterday);
    // PAST DATE
    return moment(date).format(currentDateFormat);
  }

  isTxExpanded = (tx: WalletTransaction) =>
    this.expandedTransactionIds.has(tx.id);
  isTxShowingMetadata = (tx: WalletTransaction) =>
    this.transactionsShowingMetadata.has(tx.id);
  toggleTransactionExpandedState = (tx: WalletTransaction) => {
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
  onShowMetadata = (tx: WalletTransaction) => {
    this.transactionsShowingMetadata.set(tx.id, tx);

    if (this.virtualList) {
      this.virtualList.updateTxRowHeight(tx, true, true);
    } else if (this.simpleList) {
      this.simpleList.forceUpdate();
    }
  };
  onShowMoreTransactions = (walletId: string) => {
    if (this.props.onShowMoreTransactions) {
      this.props.onShowMoreTransactions(walletId);
    }
  };
  getExpandedTransactions = () => this.expandedTransactionIds;
  renderGroup = (data: TransactionsGroup): Node => (
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Moment' is not assignable to par... Remove this comment to see the full error message
    <div className={styles.groupDate}>{this.localizedDate(data.date)}</div>
  );
  renderTransaction = (data: TransactionInfo): Node => {
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
    const txClasses = classnames([
      styles.transaction,
      isFirstInGroup ? styles.firstInGroup : null,
      isLastInGroup ? styles.lastInGroup : null,
    ]);
    const txTokens = tx.assets;
    const assetTokens = getNonZeroAssetTokens(txTokens, getAsset);
    const totalRawAssets = tx.assets.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = tx.assets.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;
    return (
      <div id={`tx-${tx.id}`} className={txClasses}>
        <Transaction
          data={tx}
          deletePendingTransaction={deletePendingTransaction}
          formattedWalletAmount={formattedWalletAmount}
          isExpanded={this.isTxExpanded(tx)}
          isShowingMetadata={this.isTxShowingMetadata(tx)}
          isLastInList={isLastInGroup}
          isRestoreActive={isRestoreActive}
          onDetailsToggled={() => this.toggleTransactionExpandedState(tx)}
          onOpenExternalLink={onOpenExternalLink}
          onShowMetadata={() => this.onShowMetadata(tx)}
          getUrlByType={getUrlByType}
          state={tx.state}
          walletId={walletId}
          isDeletingTransaction={isDeletingTransaction}
          currentTimeFormat={currentTimeFormat}
          assetTokens={assetTokens}
          hasAssetsEnabled={hasAssetsEnabled}
          isInternalAddress={isInternalAddress}
          isLoadingAssets={isLoadingAssets}
          onCopyAssetParam={onCopyAssetParam}
        />
      </div>
    );
  };
  renderItem = (row: Row) => {
    if (row instanceof TransactionsGroup) {
      return this.renderGroup(row);
    }

    if (row instanceof TransactionInfo) {
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
      (isLoadingTransactions || hasMoreToLoad) && !isRestoreActive ? (
        <LoadingSpinner
          ref={(component) => {
            this.loadingSpinner = component;
          }}
        />
      ) : null;
    const syncingTransactionsSpinner = isRestoreActive ? (
      <div className={styles.syncingTransactionsWrapper}>
        <LoadingSpinner big />
        <p className={styles.syncingTransactionsText}>
          {intl.formatMessage(messages.syncingTransactionsMessage)}
        </p>
      </div>
    ) : null;
    const buttonClasses = classnames([
      'primary',
      styles.showMoreTransactionsButton,
    ]);
    // Generate flat list with dates in-between
    const rows: Row[] = [];
    transactionsGroups.forEach((group) => {
      // First push the group into the list
      rows.push(group);
      // Followed by all transactions the tx in the group
      group.transactions.forEach((transaction, transactionIndex) => {
        const isFirstInGroup = transactionIndex === 0;
        const isLastInGroup =
          group.transactions.length === transactionIndex + 1;
        rows.push(
          new TransactionInfo({
            tx: transaction,
            isLastInGroup,
            isFirstInGroup,
          })
        );
      });
    });
    const showMoreTxButton = (
      <Button
        className={buttonClasses}
        label={intl.formatMessage(messages.showMoreTransactionsButtonLabel)} // eslint-disable-next-line react/jsx-no-bind
        onClick={this.onShowMoreTransactions.bind(this, walletId)}
        skin={ButtonSkin}
      />
    );
    if (isPreloading)
      return (
        <div className={styles.preloadingBlockWrapper}>
          <LoadingSpinner big />
        </div>
      );
    return (
      <div className={styles.component}>
        {syncingTransactionsSpinner}
        {isRenderingAsVirtualList ? (
          <VirtualTransactionList
            getExpandedTransactions={this.getExpandedTransactions}
            ref={(list) => {
              this.virtualList = list;
            }}
            renderRow={this.renderItem}
            rows={rows}
            isLoadingSpinnerShown={loadingSpinner !== null}
            isSyncingSpinnerShown={isRestoreActive}
          />
        ) : (
          <SimpleTransactionList
            ref={(list) => {
              this.simpleList = list;
            }}
            renderRow={this.renderItem}
            rows={rows}
          />
        )}
        {showMoreTransactionsButton ? showMoreTxButton : null}
        {loadingSpinner}
      </div>
    );
  }
}

export default WalletTransactionsList;
