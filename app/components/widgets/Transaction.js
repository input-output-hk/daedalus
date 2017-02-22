import React, { Component, PropTypes } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import styles from './Transaction.scss';
import adaSymbol from '../../assets/images/ada-symbol.svg';

export const transactionShape = PropTypes.shape({
  id: PropTypes.string.isRequired,
  title: PropTypes.string.isRequired,
  type: PropTypes.string.isRequired,
  amount: PropTypes.number.isRequired,
  numberOfConfirmations: PropTypes.number.isRequired,
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
  pending: {
    id: 'wallet.transaction.pendingLabel',
    defaultMessage: '!!!Pending',
    description: '"Pending" label on transaction list.'
  },
  verifications: {
    id: 'wallet.transaction.verificationsLabel',
    defaultMessage: '!!!verifications',
    description: '"verifications" label on transaction list.'
  },
});

export default class Transaction extends Component {

  static propTypes = {
    data: transactionShape,
    isLastInList: PropTypes.bool
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isExpanded: false
  };

  toggleDetails() {
    this.setState({ isExpanded: !this.state.isExpanded });
  }

  render() {
    const data = this.props.data;
    const { isExpanded } = this.state;
    const { intl } = this.context;
    let typeMessage = data.type;
    const contentStyles = classNames([
      styles.content,
      this.props.isLastInList ? styles.last : null
    ]);
    const detailsStyles = classNames([
      styles.details,
      isExpanded ? styles.expanded : styles.closed
    ]);
    if (data.type === 'adaExpend' || data.type === 'adaIncome') typeMessage = 'ada';
    const status = data.numberOfConfirmations === 0 ?
      intl.formatMessage(messages.pending) : `${data.numberOfConfirmations} ${intl.formatMessage(messages.verifications)}`;
    return (
      <div className={styles.component}>
        <div className={styles[data.type]} />
        <div className={contentStyles}>

          {/* ==== Clickable Header -> toggles details ==== */}

          <button className={styles.header} onClick={this.toggleDetails.bind(this)}>
            <div className={styles.title}>{data.type === 'adaExpend' ? 'Ada Sent' : 'Ada Received'}</div>
            <div className={styles.amount}>{data.amount}
              <img className={styles.currencySymbol} src={adaSymbol} role="presentation" />
            </div>
          </button>

          <div className={styles.details}>
            <div className={styles.type}>{intl.formatMessage(messages[typeMessage])}</div>
            <div className={styles.status}>{status}</div>
          </div>


          {/* ==== Toggleable Transaction Details ==== */}

          <div className={detailsStyles}>
            {data.exchange && data.conversionRate && (
              <div className={styles.conversion}>
                <div>
                  <h2>Exchange</h2>
                  <span>{data.exchange}</span>
                </div>
                <div className={styles.conversionRate}>
                  <h2>Conversion Rate</h2>
                  <span>{data.conversionRate}</span>
                </div>
              </div>
            )}
            <div>
              <h2>TransactionId</h2>
              <span>{data.id}</span>
            </div>
            {/*
            <div>
              <h2>Description</h2>
              <span>{data.description !== '' ? data.description : 'No description yet'}</span>
            </div>
            */}
          </div>
        </div>
      </div>
    );
  }
}
