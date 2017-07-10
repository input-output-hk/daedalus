// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import classnames from 'classnames';
import styles from './WalletTransactionsList.scss';
import Transaction from '../../widgets/Transaction';
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

@observer
export default class WalletTransactionsList extends Component {

  props: {
    transactions: Array<WalletTransaction>,
    isLoadingTransactions: boolean,
    hasMoreToLoad: boolean,
    onLoadMore: Function,
    assuranceMode: AssuranceMode,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    topShadow: false,
  };

  componentWillMount() {
    this.dateFormat = moment.localeData().longDateFormat('L');
    // Localized dateFormat:
    // English - MM/DD/YYYY
    // Japanese - YYYY/MM/DD
  }

  componentDidMount() {
    this.calcShadow();
  }

  list: HTMLElement;
  loadingSpinner: LoadingSpinner;
  dateFormat: 'MM/DD/YYYY';

  groupTransactionsByDay(transactions: Array<WalletTransaction>) {
    const { intl } = this.context;
    const groups = [];
    for (const transaction of transactions) {
      let date = moment(transaction.date).format(this.dateFormat);
      const today = moment().format(this.dateFormat);
      const yesterday = moment().subtract(1, 'days').format(this.dateFormat);
      if (date === today) date = intl.formatMessage(messages.today);
      if (date === yesterday) date = intl.formatMessage(messages.yesterday);
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
    return groups;
  }

  calcShadow() {
    const target = this.list;
    const scrollPosition = target.scrollTop;
    const topShadow = scrollPosition > 20;
    this.setState({ topShadow });
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

  handleListScroll() {
    this.calcShadow();
    const { hasMoreToLoad, isLoadingTransactions, onLoadMore } = this.props;
    if (this.isSpinnerVisible() && hasMoreToLoad && !isLoadingTransactions) {
      onLoadMore();
    }
  }

  render() {
    const { topShadow } = this.state;
    const {
      transactions,
      isLoadingTransactions,
      hasMoreToLoad,
      assuranceMode,
    } = this.props;

    const transactionsGroups = this.groupTransactionsByDay(transactions);

    const loadingSpinner = isLoadingTransactions || hasMoreToLoad ? (
      <LoadingSpinner ref={(component) => { this.loadingSpinner = component; }} />
    ) : null;

    const componentStyles = classnames([
      styles.component,
      topShadow ? styles.topShadow : null,
    ]);

    return (
      <div
        className={componentStyles}
        onScroll={this.handleListScroll.bind(this)}
        ref={(div) => { this.list = div; }}
      >
        {transactionsGroups.map((group, groupIndex) => (
          <div className={styles.group} key={groupIndex}>
            <div className={styles.groupDate}>{group.date}</div>
            <div className={styles.list}>
              {group.transactions.map((transaction, transactionIndex) => (
                <div className={styles.transaction} key={transactionIndex}>
                  <Transaction
                    data={transaction}
                    isLastInList={transactionIndex === group.transactions.length - 1}
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
