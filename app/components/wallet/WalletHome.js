// @flow
import React, { Component } from 'react';
import { observer, PropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import styles from './WalletHome.scss';
import Transaction, { transactionShape } from '../widgets/Transaction';

defineMessages({

});

const dateFormat = 'YYYY-MM-DD';

@observer
export default class WalletHome extends Component {

  static propTypes = {
    transactions: PropTypes.arrayOrObservableArrayOf(transactionShape).isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

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

  render() {
    const transactionsGroups = this.groupTransactionsByDay(this.props.transactions);
    return (
      <div className={styles.component}>
        {transactionsGroups.map((group, groupIndex) => (
          <div className={styles.group} key={groupIndex}>
            <div className={styles.groupDate}>{group.date}</div>
            <div className={styles.list}>
              {group.transactions.map((transaction, transactionIndex) => (
                <div className={styles.transaction} key={transaction.id}>
                  <Transaction
                    data={transaction}
                    isLastInList={transactionIndex === group.transactions.length - 1}
                  />
                </div>
              ))}
            </div>
          </div>
        ))}
      </div>
    );
  }

}
