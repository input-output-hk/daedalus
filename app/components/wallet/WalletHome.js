// @flow
import React, { Component } from 'react';
import { observer, PropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletHome.scss';
import Transaction, { transactionShape } from '../widgets/Transaction';

defineMessages({

});

@observer
export default class WalletHome extends Component {

  static propTypes = {
    transactions: PropTypes.arrayOrObservableArrayOf(transactionShape).isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { transactions } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.list}>
          {transactions.map((transaction, index) => (
            <div className={styles.transaction} key={transaction.id}>
              <Transaction
                data={transaction}
                isLastInList={index === transactions.length - 1}
              />
            </div>
          ))}
        </div>
      </div>
    );
  }

}
