import React, { Component, PropTypes } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
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
    description: 'Transaction type shown for credit card payments.',
  },
  ada: {
    id: 'wallet.transaction.type.ada',
    defaultMessage: '!!!ADA transaction',
    description: 'Transaction type shown for ada payments.',
  },
  exchange: {
    id: 'wallet.transaction.type.exchange',
    defaultMessage: '!!!Exchange',
    description: 'Transaction type shown for money exchanges between currencies.',
  },
  low: {
    id: 'wallet.transaction.assuranceLevel.low',
    defaultMessage: '!!!low',
    description: 'Transaction assurance level "low".',
  },
  medium: {
    id: 'wallet.transaction.assuranceLevel.medium',
    defaultMessage: '!!!medium',
    description: 'Transaction assurance level "medium".',
  },
  high: {
    id: 'wallet.transaction.assuranceLevel.high',
    defaultMessage: '!!!high',
    description: 'Transaction assurance level "high".',
  },
  assuranceLevel: {
    id: 'wallet.transaction.assuranceLevel',
    defaultMessage: '!!!Transaction assurance level',
    description: 'Transaction assurance level.',
  },
  confirmations: {
    id: 'wallet.transaction.confirmations',
    defaultMessage: '!!!confirmations',
    description: 'Transaction confirmations.',
  },
  transactionId: {
    id: 'wallet.transaction.transactionId',
    defaultMessage: '!!!Transaction ID',
    description: 'Transaction ID.',
  },
  conversionRate: {
    id: 'wallet.transaction.conversion.rate',
    defaultMessage: '!!!Conversion rate',
    description: 'Conversion rate.',
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
    const status = intl.formatMessage(messages[data.assuranceLevel]);
    return (
      <div className={styles.component}>
        <div className={styles[data.type]} />
        <div className={contentStyles}>

          {/* ==== Clickable Header -> toggles details ==== */}

          <button className={styles.header} onClick={this.toggleDetails.bind(this)}>
            <div className={styles.title}>
              {data.type === 'adaExpend' ? 'Ada Sent' : 'Ada Received'}
            </div>
            <div className={styles.amount}>{data.amount}
              <img className={styles.currencySymbol} src={adaSymbol} role="presentation" />
            </div>
          </button>

          <div className={styles.details}>
            <div className={styles.type}>
              {intl.formatMessage(messages[typeMessage])}
              , {moment(data.date).format('hh:mm:ss A')}
              {/* TODO: Use locale to format the date */}
            </div>
            <div className={styles[data.assuranceLevel]}>{status}</div>
          </div>


          {/* ==== Toggleable Transaction Details ==== */}

          <div className={detailsStyles}>
            {data.exchange && data.conversionRate && (
              <div className={styles.conversion}>
                <div>
                  <h2>{intl.formatMessage(messages.exchange)}</h2>
                  <span>{data.exchange}</span>
                </div>
                <div className={styles.conversionRate}>
                  <h2>{intl.formatMessage(messages.conversionRate)}</h2>
                  <span>{data.conversionRate}</span>
                </div>
              </div>
            )}
            <div>
              <h2>{intl.formatMessage(messages.assuranceLevel)}</h2>
              <span>
                <span className={styles.assuranceLevel}>{status}</span>
                . {data.numberOfConfirmations} {intl.formatMessage(messages.confirmations)}.
              </span>
              <h2>{intl.formatMessage(messages.transactionId)}</h2>
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
