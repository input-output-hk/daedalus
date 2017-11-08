import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import SvgInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './Transaction.scss';
import TransactionTypeIcon from './TransactionTypeIcon';
import adaSymbol from '../../assets/images/ada-symbol.inline.svg';
import WalletTransaction, { transactionStates } from '../../domain/WalletTransaction';
import { assuranceLevels } from '../../config/transactionAssuranceConfig';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';
import type { TransactionState } from '../../domain/WalletTransaction';

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
  adaSent: {
    id: 'wallet.transaction.adaSent',
    defaultMessage: '!!!Ada sent',
    description: 'Label "Ada sent" for the transaction.',
  },
  adaReceived: {
    id: 'wallet.transaction.adaReceived',
    defaultMessage: '!!!Ada received',
    description: 'Label "Ada received" for the transaction.',
  },
  fromAddresses: {
    id: 'wallet.transaction.addresses.from',
    defaultMessage: '!!!From addresses',
    description: 'From addresses',
  },
  toAddresses: {
    id: 'wallet.transaction.addresses.to',
    defaultMessage: '!!!To addresses',
    description: 'To addresses',
  },
});

const assuranceLevelTranslations = defineMessages({
  [assuranceLevels.LOW]: {
    id: 'wallet.transaction.assuranceLevel.low',
    defaultMessage: '!!!low',
    description: 'Transaction assurance level "low".',
  },
  [assuranceLevels.MEDIUM]: {
    id: 'wallet.transaction.assuranceLevel.medium',
    defaultMessage: '!!!medium',
    description: 'Transaction assurance level "medium".',
  },
  [assuranceLevels.HIGH]: {
    id: 'wallet.transaction.assuranceLevel.high',
    defaultMessage: '!!!high',
    description: 'Transaction assurance level "high".',
  },
});

const stateTranslations = defineMessages({
  [transactionStates.PENDING]: {
    id: 'wallet.transaction.state.pending',
    defaultMessage: '!!!Transaction pending',
    description: 'Transaction state "pending"',
  },
  [transactionStates.FAILED]: {
    id: 'wallet.transaction.state.failed',
    defaultMessage: '!!!Transaction failed',
    description: 'Transaction state "pending"',
  },
});

export default class Transaction extends Component {

  props: {
    data: WalletTransaction,
    state: TransactionState,
    assuranceLevel: string,
    isLastInList: boolean,
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
    const { isLastInList, state, assuranceLevel } = this.props;
    const { isExpanded } = this.state;
    const { intl } = this.context;
    const isFailedTransaction = state === transactionStates.FAILED;
    let typeMessage = data.type;

    const componentStyles = classNames([
      styles.component,
      isFailedTransaction ? styles.failed : null
    ]);

    const contentStyles = classNames([
      styles.content,
      isLastInList ? styles.last : null
    ]);

    const detailsStyles = classNames([
      styles.details,
      isExpanded ? styles.expanded : styles.closed
    ]);

    if (data.type === 'expend' || data.type === 'income') typeMessage = 'ada';

    const status = intl.formatMessage(assuranceLevelTranslations[assuranceLevel]);

    return (
      <div className={componentStyles}>

        {/* ==== Clickable Header -> toggles details ==== */}
        <div className={styles.toggler} onClick={this.toggleDetails.bind(this)} role="presentation" aria-hidden>
          <TransactionTypeIcon
            iconType={isFailedTransaction ? transactionStates.FAILED : data.type}
          />

          <div className={styles.togglerContent}>
            <div className={styles.header}>
              <div className={styles.title}>
                {data.type === 'expend' ?
                  intl.formatMessage(messages.adaSent) :
                  intl.formatMessage(messages.adaReceived)
                }
              </div>
              <div className={styles.amount}>{data.amount.toFormat(DECIMAL_PLACES_IN_ADA)}
                <SvgInline svg={adaSymbol} className={styles.currencySymbol} />
              </div>
            </div>

            <div className={styles.details}>
              <div className={styles.type}>
                {intl.formatMessage(messages[typeMessage])}
                , {moment(data.date).format('hh:mm:ss A')}
              </div>

              {state === transactionStates.OK ? (
                <div className={styles[assuranceLevel]}>{status}</div>
              ) : (
                <div className={styles[`${state}Label`]}>
                  {intl.formatMessage(stateTranslations[state])}
                </div>
              )}
            </div>
          </div>
        </div>

        {/* ==== Toggleable Transaction Details ==== */}
        <div className={contentStyles}>
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
              <h2>{intl.formatMessage(messages.fromAddresses)}</h2>
              {data.addresses.from.map((address, addressIndex) => (
                <span key={`${data.id}-from-${address}-${addressIndex}`} className={styles.address}>{address}</span>
              ))}
              <h2>{intl.formatMessage(messages.toAddresses)}</h2>
              {data.addresses.to.map((address, addressIndex) => (
                <span key={`${data.id}-to-${address}-${addressIndex}`} className={styles.address}>{address}</span>
              ))}
              <h2>{intl.formatMessage(messages.assuranceLevel)}</h2>

              {state === transactionStates.OK ? (
                <span>
                  <span className={styles.assuranceLevel}>{status}</span>
                  . {data.numberOfConfirmations} {intl.formatMessage(messages.confirmations)}.
                </span>
              ) : null}

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
