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
    return (
      <div className={styles.component}>
        <div className={styles.list}>
          {this.props.transactions.map((transaction) => (
            <div className={styles.transaction} key={transaction.id}>
              <Transaction data={transaction} />
            </div>
          ))}
        </div>
      </div>
    );
  }

}
