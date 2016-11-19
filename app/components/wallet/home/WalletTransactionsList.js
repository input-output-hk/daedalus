// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import classnames from 'classnames';
import styles from './WalletTransactionsList.scss';
import Transaction, { transactionShape } from '../../widgets/Transaction';

defineMessages({

});

const dateFormat = 'YYYY-MM-DD';

@observer
export default class WalletTransactionsList extends Component {

  static propTypes = {
    transactions: MobxPropTypes.arrayOrObservableArrayOf(transactionShape).isRequired,
    isLoadingTransactions: PropTypes.bool.isRequired,
    hasMoreToLoad: PropTypes.bool.isRequired,
    onLoadMore: PropTypes.func.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    topShadow: false,
    bottomShadow: false,
  };

  componentDidMount() {
    this.calcShadows();
  }

  list: HTMLElement;
  loadingSpinner: HTMLElement;

  groupTransactionsByDay(transactions:[Object]) {
    const groups = [];
    for (const transaction of transactions) {
      let date = moment(transaction.date).format(dateFormat);
      const today = moment().format(dateFormat);
      const yesterday = moment().subtract(1, 'days').format(dateFormat);
      if (date === today) date = 'Today';
      if (date === yesterday) date = 'Yesterday';
      let group = groups.find((g) => g.date === date);
      if (!group) {
        group = { date, transactions: [] };
        groups.push(group);
      }
      group.transactions.push(transaction);
    }
    return groups;
  }

  calcShadows() {
    const target = this.list;
    const scrollPosition = target.scrollTop;
    const maxScrollPosition = target.scrollHeight - target.offsetHeight;
    let topShadow = false;
    let bottomShadow = false;
    if (scrollPosition > 0) topShadow = true;
    if (scrollPosition < maxScrollPosition) bottomShadow = true;
    this.setState({ topShadow, bottomShadow });
  }

  isSpinnerVisible() {
    if (this.loadingSpinner == null) return false;
    const spinnerRect = this.loadingSpinner.getBoundingClientRect();
    const viewHeight = Math.max(document.documentElement.clientHeight, window.innerHeight);
    return !(spinnerRect.bottom < 0 || spinnerRect.top - viewHeight >= 0);
  }

  handleListScroll() {
    this.calcShadows();
    const { hasMoreToLoad, isLoadingTransactions, onLoadMore } = this.props;
    if (this.isSpinnerVisible() && hasMoreToLoad && !isLoadingTransactions) {
      onLoadMore();
    }
  }

  render() {
    const { topShadow, bottomShadow } = this.state;
    const { transactions, isLoadingTransactions, hasMoreToLoad } = this.props;
    const transactionsGroups = this.groupTransactionsByDay(transactions);

    let shadowStyle = null;
    if (topShadow && bottomShadow) {
      shadowStyle = styles.bothShadows;
    } else {
      if (topShadow) shadowStyle = styles.onlyTopShadow;
      if (bottomShadow) shadowStyle = styles.onlyBottomShadow;
    }
    const componentStyles = classnames([styles.component, shadowStyle]);
    const loadingSpinner = isLoadingTransactions || hasMoreToLoad ? (
      <div className={styles.spinner} ref={(div) => { this.loadingSpinner = div; }} />
    ) : null;
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
