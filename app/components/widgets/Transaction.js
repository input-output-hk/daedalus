import React, { Component, PropTypes } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './Transaction.scss';

export const transactionShape = PropTypes.shape({
  id: PropTypes.string.isRequired,
  title: PropTypes.string.isRequired,
  type: PropTypes.string.isRequired,
  amount: PropTypes.number.isRequired,
  currency: PropTypes.string.isRequired,
  date: PropTypes.instanceOf(Date),
});

const messages = defineMessages({
  card: {
    id: 'wallet.transaction.type.card',
    defaultMessage: '!!!Card payment',
    description: 'Transaction type shown for credit card payments.'
  },
  ada: {
    id: 'wallet.transaction.type.ada',
    defaultMessage: '!!!ADA transaction',
    description: 'Transaction type shown for ada payments.'
  },
  exchange: {
    id: 'wallet.transaction.type.exchange',
    defaultMessage: '!!!Exchange transaction',
    description: 'Transaction type shown for money exchanges between currencies.'
  },
});

export default class Transaction extends Component {

  static propTypes = {
    data: transactionShape
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { title, type, amount, currency } = this.props.data;
    const { intl } = this.context;
    let typeMessage = type;
    if (type === 'adaExpend' || type === 'adaIncome') typeMessage = 'ada';
    return (
      <div className={styles.component}>
        <div className={styles[type]} />
        <div className={styles.details}>
          <div className={styles.title}>{title}</div>
          <div className={styles.type}>{intl.formatMessage(messages[typeMessage])}</div>
          <div className={styles.amount}>{amount} {currency}</div>
        </div>
      </div>
    );
  }
}
