// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import styles from './WalletTransactionsList.scss';
import Transaction from './Transaction';
import WalletTransaction from '../../../domain/WalletTransaction';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import type { AssuranceMode } from '../../../types/transactionAssuranceTypes';

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
});

const dateFormat = 'YYYY-MM-DD';

type Props = {
  transactions: Array<WalletTransaction>,
  isLoadingTransactions: boolean,
  hasMoreToLoad: boolean,
  onLoadMore: Function,
  assuranceMode: AssuranceMode,
  walletId: string,
};

@observer
export default class WalletTransactionsList extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentWillMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
    // Localized dateFormat:
    // English - MM/DD/YYYY
    // Japanese - YYYY/MM/DD
  }

  list: HTMLElement;
  loadingSpinner: ?LoadingSpinner;
  localizedDateFormat: 'MM/DD/YYYY';

  groupTransactionsByDay(transactions: Array<WalletTransaction>) {
    const groups = [];
    for (const transaction of transactions) {
      const date = moment(transaction.date).format(dateFormat);
      let group = groups.find((g) => g.date === date);
      if (!group) {
        group = { date, transactions: [] };
        groups.push(group);
      }
      group.transactions.push(transaction);
    }
    for (const group of groups) {
      group.transactions.sort((a, b) => b.date.getTime() - a.date.getTime());
    }
    return groups.sort(
      (a, b) => b.transactions[0].date.getTime() - a.transactions[0].date.getTime()
    );
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
    const today = moment().format(dateFormat);
    if (date === today) return intl.formatMessage(messages.today);
    const yesterday = moment().subtract(1, 'days').format(dateFormat);
    if (date === yesterday) return intl.formatMessage(messages.yesterday);
    return moment(date).format(this.localizedDateFormat);
  }

  render() {
    const {
      transactions,
      isLoadingTransactions,
      hasMoreToLoad,
      assuranceMode,
      walletId,
    } = this.props;

    const transactionsGroups = this.groupTransactionsByDay(transactions);

    const loadingSpinner = isLoadingTransactions || hasMoreToLoad ? (
      <LoadingSpinner ref={(component) => { this.loadingSpinner = component; }} />
    ) : null;

    return (
      <div className={styles.component}>
        {transactionsGroups.map((group, groupIndex) => (
          <div className={styles.group} key={walletId + '-' + groupIndex}>
            <div className={styles.groupDate}>{this.localizedDate(group.date)}</div>
            <div className={styles.list}>
              {group.transactions.map((transaction, transactionIndex) => (
                <div key={walletId + '-' + transaction.id}>
                  <Transaction
                    data={transaction}
                    isLastInList={transactionIndex === group.transactions.length - 1}
                    state={transaction.state}
                    assuranceLevel={transaction.getAssuranceLevelForMode(assuranceMode)}
                  />
                </div>
              ))}
            </div>
          </div>
        ))}
        {loadingSpinner}
      </div>
    );
  }

}
