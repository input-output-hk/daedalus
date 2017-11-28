import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import SvgInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './Transaction.scss';
import TransactionTypeIcon from './TransactionTypeIcon';
import adaSymbol from '../../../assets/images/ada-symbol.inline.svg';
import etcSymbol from '../../../assets/images/etc-symbol.inline.svg';
import WalletTransaction, { transactionStates, transactionTypes } from '../../../domain/WalletTransaction';
import { assuranceLevels } from '../../../config/transactionAssuranceConfig';
import { environmentSpecificMessages } from '../../../i18n/global-messages';
import type { TransactionState } from '../../../domain/WalletTransaction';
import environment from '../../../environment';
import resolver from '../../../utils/imports';

const { formattedWalletAmount } = resolver('utils/formatters');

const messages = defineMessages({
  card: {
    id: 'wallet.transaction.type.card',
    defaultMessage: '!!!Card payment',
    description: 'Transaction type shown for credit card payments.',
  },
  type: {
    id: 'wallet.transaction.type',
    defaultMessage: '!!!{currency} transaction',
    description: 'Transaction type shown for {currency} transactions.',
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
  sent: {
    id: 'wallet.transaction.sent',
    defaultMessage: '!!!{currency} sent',
    description: 'Label "{currency} sent" for the transaction.',
  },
  received: {
    id: 'wallet.transaction.received',
    defaultMessage: '!!!{currency} received',
    description: 'Label "{currency} received" for the transaction.',
  },
  fromAddress: {
    id: 'wallet.transaction.address.from',
    defaultMessage: '!!!From address',
    description: 'From address',
  },
  fromAddresses: {
    id: 'wallet.transaction.addresses.from',
    defaultMessage: '!!!From addresses',
    description: 'From addresses',
  },
  toAddress: {
    id: 'wallet.transaction.address.to',
    defaultMessage: '!!!To address',
    description: 'To address',
  },
  toAddresses: {
    id: 'wallet.transaction.addresses.to',
    defaultMessage: '!!!To addresses',
    description: 'To addresses',
  },
  transactionAmount: {
    id: 'wallet.transaction.transactionAmount',
    defaultMessage: '!!!Transaction amount',
    description: 'Transaction amount.',
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

type Props = {
  data: WalletTransaction,
  state: TransactionState,
  assuranceLevel: string,
  isLastInList: boolean,
};

type State = {
  isExpanded: boolean,
};

export default class Transaction extends Component<Props, State> {

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

    const status = intl.formatMessage(assuranceLevelTranslations[assuranceLevel]);
    const currency = intl.formatMessage(environmentSpecificMessages[environment.API].currency);
    const symbol = environment.isAdaApi() ? adaSymbol : etcSymbol;

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
                {data.type === transactionTypes.EXPEND ?
                  intl.formatMessage(messages.sent, { currency }) :
                  intl.formatMessage(messages.received, { currency })
                }
              </div>
              <div className={styles.amount}>
                {
                  // hide currency (we are showing symbol instead)
                  formattedWalletAmount(data.amount, false)
                }
                <SvgInline svg={symbol} className={styles.currencySymbol} />
              </div>
            </div>

            <div className={styles.details}>
              <div className={styles.type}>
                {intl.formatMessage(messages.type, { currency })}
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
              <h2>
                {intl.formatMessage(messages[
                  environment.isEtcApi() ? 'fromAddress' : 'fromAddresses'
                ])}
              </h2>
              {data.addresses.from.map((address, addressIndex) => (
                <span key={`${data.id}-from-${address}-${addressIndex}`} className={styles.address}>{address}</span>
              ))}
              <h2>
                {intl.formatMessage(messages[
                  environment.isEtcApi() ? 'toAddress' : 'toAddresses'
                ])}
              </h2>
              {data.addresses.to.map((address, addressIndex) => (
                <span key={`${data.id}-to-${address}-${addressIndex}`} className={styles.address}>{address}</span>
              ))}

              {environment.isAdaApi() ? (
                <div className={styles.row}>
                  <h2>{intl.formatMessage(messages.assuranceLevel)}</h2>
                  {state === transactionStates.OK ? (
                    <span>
                      <span className={styles.assuranceLevel}>{status}</span>
                      . {data.numberOfConfirmations} {intl.formatMessage(messages.confirmations)}.
                    </span>
                  ) : null}
                </div>
              ) : null}

              {environment.isEtcApi() ? (
                <div className={styles.row}>
                  <h2>{intl.formatMessage(messages.transactionAmount)}</h2>
                  <span>
                    {
                      // show currency and use long format (e.g. in ETC show all decimal places)
                      formattedWalletAmount(data.amount, true, true)
                    }
                  </span>
                </div>
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
