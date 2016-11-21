// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletNoTransactions.scss';

const messages = defineMessages({
  noTransactions: {
    id: 'wallet.transactions.no.transactions',
    defaultMessage: '!!!No transactions',
    description: 'Message "No transactions" when wallet has no transactions or transaction search returns zero results.'
  }
});


@observer
export default class WalletNoTransactions extends Component {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    return (
      <div className={styles.component}>
        <div className={styles.label}>{intl.formatMessage(messages.noTransactions)}</div>
      </div>

    );
  }

}
