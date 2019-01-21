// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import { AutoSizer, List } from 'react-virtualized';
import styles from './WalletTransactionsList.scss';
import Transaction from './Transaction';
import { WalletTransaction } from '../../../domains/WalletTransaction';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import { DEVELOPMENT } from '../../../../../common/types/environment.types';
import type { WalletAssuranceMode } from '../../../api/wallets/types';

const messages = defineMessages({
  today: {
    id: 'wallet.summary.page.todayLabel',
    defaultMessage: '!!!Today',
    description: 'Label for the "Today" label on the wallet summary page.',
  },
  yesterday: {
    id: 'wallet.summary.page.yesterdayLabel',
    defaultMessage: '!!!Yesterday',
    description: 'Label for the "Yesterday" label on the wallet summary page.',
  },
  showMoreTransactionsButtonLabel: {
    id: 'wallet.summary.page.showMoreTransactionsButtonLabel',
    defaultMessage: '!!!Show more transactions',
    description: 'Label for the "Show more transactions" button on the wallet summary page.',
  },
  syncingTransactionsMessage: {
    id: 'wallet.summary.page.syncingTransactionsMessage',
    defaultMessage: '!!!Your transaction history for this wallet is being synced with the blockchain.',
    description: 'Syncing transactions message on async wallet restore.',
  },
});

type Props = {
  transactions: Array<WalletTransaction>,
  isLoadingTransactions: boolean,
  isRestoreActive: boolean,
  hasMoreToLoad: boolean,
  assuranceMode: WalletAssuranceMode,
  walletId: string,
  formattedWalletAmount: Function,
  network: string,
  showMoreTransactionsButton?: boolean,
  onShowMoreTransactions?: Function,
  onOpenExternalLink?: Function,
};


type State = {
  expandedTransactions: Array<WalletTransaction>,
};

type TransactionsGroup = { date: moment.Moment, transactions: Array<WalletTransaction>};
type GroupMarker = 'GROUP';
type TransactionInfo = {
  tx: WalletTransaction,
  isLastInGroup: boolean,
  isFirstInGroup: boolean,
};
type Row = TransactionInfo | GroupMarker;
type RowHeight = { height: number };

const DATE_FORMAT = 'YYYY-MM-DD';
const GROUP_MARKER: GroupMarker = 'GROUP';
const GROUP_DATE_HEIGHT = 30;
const TX_ROW_HEIGHT = 86;
const TX_ROW_HEIGHT_EXPANDED = 310;
const TX_LAST_IN_GROUP_MARGIN = 20;
const TX_HEIGHT_PER_ADDRESS = 38;

@observer
export default class WalletTransactionsList extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    network: DEVELOPMENT,
    showMoreTransactionsButton: false,
    onShowMoreTransactions: () => {},
    onOpenExternalLink: () => {},
  };

  state = {
    expandedTransactions: [],
  };

  list: List;
  loadingSpinner: ?LoadingSpinner;
  localizedDateFormat: 'MM/DD/YYYY';

  componentWillMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
    // Localized dateFormat:
    // English - MM/DD/YYYY
    // Japanese - YYYY/MM/DD
  }

  groupTransactionsByDay(transactions: Array<WalletTransaction>): Array<TransactionsGroup> {
    const groups: Array<TransactionsGroup> = [];
    for (const transaction of transactions) {
      const date = moment(transaction.date);
      let group = groups.find((g) => (
        g.date.format(DATE_FORMAT) === date.format(DATE_FORMAT)
      ));
      if (!group) {
        group = { date, transactions: [] };
        groups.push(group);
      }
      group.transactions.push(transaction);
    }
    return groups.sort((a: TransactionsGroup, b: TransactionsGroup) => (
      b.date.valueOf() - a.date.valueOf()
    ));
  }

  isSpinnerVisible() {
    const spinner = this.loadingSpinner;
    if (spinner == null || spinner.root == null) return false;
    const spinnerRect = spinner.root.getBoundingClientRect();
    const clientHeight = document.documentElement ? document.documentElement.clientHeight : 0;
    const windowHeight = window.innerHeight;
    const viewHeight = Math.max(clientHeight, windowHeight);
    return !(spinnerRect.bottom < 0 || spinnerRect.top - viewHeight >= 0);
  }

  localizedDate(date: string) {
    const { intl } = this.context;
    // TODAY
    const today = moment().format(DATE_FORMAT);
    if (date === today) return intl.formatMessage(messages.today);
    // YESTERDAY
    const yesterday = moment().subtract(1, 'days').format(DATE_FORMAT);
    if (date === yesterday) return intl.formatMessage(messages.yesterday);
    // PAST DATE
    return moment(date).format(this.localizedDateFormat);
  }

  isTxExpanded = (tx: WalletTransaction) => (
    !!this.state.expandedTransactions.find(t => t.id === tx.id)
  );

  calculateRowHeights = (rows: Row[]): RowHeight[] => (
    rows.map((tx, index) => ({
      height: this.calculateRowHeight(rows, index)
    }))
  );

  calculateRowHeight = (rows: Row[], index: number) => {
    const row = rows[index];
    if (row === GROUP_MARKER) {
      return GROUP_DATE_HEIGHT;
    }
    if (row.tx instanceof WalletTransaction) {
      // Calculate the height of a transaction row:
      const { addresses } = row.tx;
      const isExpanded = this.isTxExpanded(row.tx);
      const baseHeight = isExpanded ? TX_ROW_HEIGHT_EXPANDED : TX_ROW_HEIGHT;
      const totalAddresses = addresses.from.length + addresses.to.length;

      // Extra height for addresses when expanded
      const heightForAddresses = isExpanded ? totalAddresses * TX_HEIGHT_PER_ADDRESS : 0;

      // Add spacing for the next date header to the last transaction in a group
      const headerSpacing = row.isLastInGroup ? TX_LAST_IN_GROUP_MARGIN : 0;

      return baseHeight + heightForAddresses + headerSpacing;
    }
    return 0;
  };

  registerTxAsExpanded = (tx: WalletTransaction) => {
    if (!this.isTxExpanded(tx)) {
      const existing = this.state.expandedTransactions;
      this.setState({
        expandedTransactions: existing.concat([tx]),
      });
    }
  };

  removeTxFromExpanded = (tx: WalletTransaction) => {
    if (this.isTxExpanded(tx)) {
      const existing = this.state.expandedTransactions;
      this.setState({
        expandedTransactions: existing.filter((t) => t.id !== tx.id)
      });
    }
  };

  toggleTransactionExpandedState = (tx: WalletTransaction) => {
    if (this.isTxExpanded(tx)) {
      this.removeTxFromExpanded(tx);
    } else {
      this.registerTxAsExpanded(tx);
    }
    if (this.list) {
      const { transactions } = this.props;
      this.list.recomputeRowHeights(transactions.indexOf(tx) || 0);
    }
  };

  render() {
    const {
      assuranceMode,
      formattedWalletAmount,
      hasMoreToLoad,
      isLoadingTransactions,
      isRestoreActive,
      network,
      onOpenExternalLink,
      showMoreTransactionsButton,
      transactions,
      walletId,
    } = this.props;

    const { intl } = this.context;
    const transactionsGroups = this.groupTransactionsByDay(transactions);

    const loadingSpinner = (isLoadingTransactions || hasMoreToLoad) && !isRestoreActive ? (
      <LoadingSpinner ref={(component) => { this.loadingSpinner = component; }} />
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
    const listItemsData: Row[] = [];
    const listItems = [];
    transactionsGroups.forEach((group) => {
      listItemsData.push(GROUP_MARKER);
      listItems.push(
        <div className={styles.groupDate}>
          {this.localizedDate(group.date)}
        </div>
      );

      group.transactions.forEach((transaction, transactionIndex) => {
        const isFirstInGroup = (transactionIndex === 0);
        const isLastInGroup = (group.transactions.length === (transactionIndex + 1));
        listItemsData.push({ tx: transaction, isLastInGroup, isFirstInGroup });
        listItems.push(
          <Transaction
            assuranceLevel={transaction.getAssuranceLevelForMode(assuranceMode)}
            data={transaction}
            formattedWalletAmount={formattedWalletAmount}
            isExpanded={this.isTxExpanded(transaction)}
            isLastInList={isLastInGroup}
            isRestoreActive={isRestoreActive}
            network={network}
            onDetailsToggled={() => this.toggleTransactionExpandedState(transaction)}
            onOpenExternalLink={onOpenExternalLink}
            state={transaction.state}
          />
        );
      });
    });

    const rowHeights = this.calculateRowHeights(listItemsData);

    // Renders a single row within the virtual list
    const rowRenderer = ({
      key, // Unique key within array of rows
      index, // Index of row within collection
      style // Style object to be applied to row (to position it)
    }: { key: string, index: number, style: string }) => {
      const rowData = listItemsData[index];
      const isTxRow = rowData !== GROUP_MARKER;
      const rowClasses = isTxRow ? (
        classnames([
          styles.transaction,
          rowData.isLastInGroup ? styles.lastInGroup : null,
          rowData.isFirstInGroup ? styles.firstInGroup : null,
        ])
      ) : null;
      return (
        <div key={key} style={style}>
          <div className={rowClasses}>
            {listItems[index]}
          </div>
        </div>
      );
    };

    // Virtual list that can efficiently render any number of items:
    const renderedTransactionsList = (
      <AutoSizer>
        {({ width, height }) => (
          <List
            className={styles.list}
            ref={(list) => { if (list) this.list = list; }}
            width={width}
            height={height}
            rowCount={listItems.length}
            rowHeight={({ index }) => (
              rowHeights[index] ? rowHeights[index].height : 100
            )}
            rowRenderer={rowRenderer}
            style={{ overflowY: 'scroll' }}
          />
        )}
      </AutoSizer>
    );

    const showMoreTxButton = (
      <Button
        className={buttonClasses}
        label={intl.formatMessage(messages.showMoreTransactionsButtonLabel)}
        onClick={this.onShowMoreTransactions.bind(this, walletId)}
        skin={ButtonSkin}
      />
    );

    return (
      <div className={styles.component}>
        {syncingTransactionsSpinner}
        {showMoreTransactionsButton ? (
          <Fragment>
            <div className={styles.listWrapper}>
              {renderedTransactionsList}
            </div>
            {showMoreTxButton}
          </Fragment>
        ) : (
          renderedTransactionsList
        )}
        {loadingSpinner}
      </div>
    );
  }

  onShowMoreTransactions = (walletId: string) => {
    if (this.props.onShowMoreTransactions) {
      this.props.onShowMoreTransactions(walletId);
    }
  };
}
