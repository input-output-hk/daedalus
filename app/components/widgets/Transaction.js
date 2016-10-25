import React, { Component, PropTypes } from 'react';
import styles from './Transaction.scss';

export const transactionShape = PropTypes.shape({
  id: PropTypes.string.isRequired,
  title: PropTypes.string.isRequired,
  type: PropTypes.string.isRequired,
  amount: PropTypes.number.isRequired,
  currency: PropTypes.string.isRequired,
  date: PropTypes.instanceOf(Date),
});

export default class Transaction extends Component {

  static propTypes = {
    data: transactionShape
  };

  render() {
    const { title, type, amount, currency } = this.props.data;
    return (
      <div className={styles.component}>
        <div className={styles[type]} />
        <div className={styles.details}>
          <div className={styles.title}>{title}</div>
          <div className={styles.type}>{type}</div>
          <div className={styles.amount}>{amount} {currency}</div>
        </div>
      </div>
    );
  }
}
