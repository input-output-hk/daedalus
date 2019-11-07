// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import CancelTransactionButton from './CancelTransactionButton';
import styles from './Transaction.scss';
import TransactionTypeIcon from './TransactionTypeIcon.js';
import adaSymbol from '../../../assets/images/ada-symbol.inline.svg';
import arrow from '../../../assets/images/collapse-arrow.inline.svg';
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';
import {
  TransactionStates,
  TransactionTypes,
  WalletTransaction,
} from '../../../domains/WalletTransaction';
import globalMessages from '../../../i18n/global-messages';
import type { TransactionState } from '../../../domains/WalletTransaction';
import { getNetworkExplorerUrl } from '../../../utils/network';
import { PENDING_LIMIT } from '../../../config/txnsConfig';

/* eslint-disable consistent-return */

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
    description:
      'Transaction type shown for money exchanges between currencies.',
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

const stateTranslations = defineMessages({
  [TransactionStates.OK]: {
    id: 'wallet.transaction.state.confirmed',
    defaultMessage: '!!!Transaction confirmed',
    description: 'Transaction state "confirmed"',
  },
  [TransactionStates.PENDING]: {
    id: 'wallet.transaction.state.pending',
    defaultMessage: '!!!Transaction pending',
    description: 'Transaction state "pending"',
  },
});

type Props = {
  data: WalletTransaction,
  deletePendingTransaction: Function,
  state: TransactionState,
  isExpanded: boolean,
  isRestoreActive: boolean,
  isLastInList: boolean,
  formattedWalletAmount: Function,
  network: string,
  onDetailsToggled: ?Function,
  onOpenExternalLink: ?Function,
  walletId: string,
};

export default class Transaction extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  toggleDetails() {
    const { onDetailsToggled } = this.props;
    if (onDetailsToggled) onDetailsToggled();
  }

  handleOpenExplorer(type: string, param: string, e: Event) {
    const { onOpenExternalLink, network } = this.props;
    if (onOpenExternalLink) {
      e.stopPropagation();
      const link = `${getNetworkExplorerUrl(network)}/${type}/${param}`;
      onOpenExternalLink(link);
    }
  }

  handleOpenSupportArticle = () => {
    const { onOpenExternalLink } = this.props;
    const articleUrl = 'https://daedaluswallet.io';
    if (!onOpenExternalLink) return null;
    return onOpenExternalLink(articleUrl);
  };

  deletePendingTransaction() {
    const { data, walletId } = this.props;
    const { id: transactionId, state } = data;

    if (state !== 'pending') {
      return null;
    }

    return this.props.deletePendingTransaction({
      walletId,
      transactionId,
    });
  }

  getTimePending = (txnDate: Date): number => {
    // right now (milliseconds) minus txn created_at date (milliseconds)
    const NOW = moment().valueOf();
    const TXN_PENDING_SINCE = moment(txnDate).valueOf();
    return NOW - TXN_PENDING_SINCE;
  };

  renderTxnStateTag = () => {
    const { intl } = this.context;
    const {
      data: { date },
      isRestoreActive,
      state,
    } = this.props;

    if (isRestoreActive || !date) return;

    const PENDING_SINCE = this.getTimePending(date);
    const isPendingTxn = state === TransactionStates.PENDING;
    const pendingTimedOut = isPendingTxn && PENDING_SINCE > PENDING_LIMIT;
    const styleLabel = pendingTimedOut
      ? `${state}WarningLabel`
      : `${state}Label`;

    return (
      <div className={styles[styleLabel]}>
        {intl.formatMessage(stateTranslations[state])}
      </div>
    );
  };

  render() {
    const {
      data,
      isLastInList,
      state,
      formattedWalletAmount,
      onOpenExternalLink,
      isExpanded,
    } = this.props;
    const { intl } = this.context;

    const canOpenExplorer = onOpenExternalLink;

    const isPendingTransaction = state === TransactionStates.PENDING;

    const txnDate = data.date ? data.date : new Date();

    const componentStyles = classNames([
      styles.component,
      isExpanded ? 'Transaction_expanded' : null,
    ]);

    const contentStyles = classNames([
      styles.content,
      isLastInList ? styles.last : null,
      isExpanded ? styles.contentExpanded : null,
    ]);

    const detailsStyles = classNames([
      styles.details,
      canOpenExplorer ? styles.clickable : null,
      isExpanded ? styles.detailsExpanded : styles.detailsClosed,
    ]);

    const arrowStyles = classNames([
      styles.arrow,
      isExpanded ? styles.arrowExpanded : null,
    ]);

    const currency = intl.formatMessage(globalMessages.currency);
    const symbol = adaSymbol;

    const iconType = isPendingTransaction
      ? TransactionStates.PENDING
      : data.type;

    return (
      <div
        onClick={this.toggleDetails.bind(this)}
        className={componentStyles}
        role="presentation"
        aria-hidden
      >
        <div className={styles.toggler}>
          <TransactionTypeIcon txnDate={txnDate} iconType={iconType} />

          <div className={styles.togglerContent}>
            <div className={styles.header}>
              <div className={styles.title}>
                {data.type === TransactionTypes.EXPEND
                  ? intl.formatMessage(messages.sent, { currency })
                  : intl.formatMessage(messages.received, { currency })}
              </div>
              <div className={styles.amount}>
                {// hide currency (we are showing symbol instead)
                formattedWalletAmount(data.amount, false)}
                <SVGInline svg={symbol} className={styles.currencySymbol} />
              </div>
            </div>

            <div className={styles.details}>
              <div className={styles.type}>
                {intl.formatMessage(messages.type, { currency })},{' '}
                {moment(data.date).format('hh:mm:ss A')}
              </div>
              {this.renderTxnStateTag()}
            </div>
          </div>
        </div>

        {/* ==== Toggleable Transaction Details ==== */}
        <div className={contentStyles}>
          <div
            className={detailsStyles}
            onClick={event => event.stopPropagation()}
            role="presentation"
            aria-hidden
          >
            <div>
              <h2>{intl.formatMessage(messages.fromAddresses)}</h2>
              {data.addresses.from.map((address, addressIndex) => (
                <div
                  // eslint-disable-next-line react/no-array-index-key
                  key={`${data.id}-from-${address}-${addressIndex}`}
                  className={styles.addressRow}
                >
                  <span
                    role="presentation"
                    aria-hidden
                    className={styles.address}
                    onClick={this.handleOpenExplorer.bind(
                      this,
                      'address',
                      address
                    )}
                  >
                    {address}
                    <SVGInline svg={externalLinkIcon} />
                  </span>
                </div>
              ))}

              <h2>{intl.formatMessage(messages.toAddresses)}</h2>
              {data.addresses.to.map((address, addressIndex) => (
                <div
                  // eslint-disable-next-line react/no-array-index-key
                  key={`${data.id}-to-${address}-${addressIndex}`}
                  className={styles.addressRow}
                >
                  <span
                    role="presentation"
                    aria-hidden
                    className={styles.address}
                    onClick={this.handleOpenExplorer.bind(
                      this,
                      'address',
                      address
                    )}
                  >
                    {address}
                    <SVGInline svg={externalLinkIcon} />
                  </span>
                </div>
              ))}

              <h2>{intl.formatMessage(messages.transactionId)}</h2>
              <div className={styles.transactionIdRow}>
                <span
                  role="presentation"
                  aria-hidden
                  className={styles.transactionId}
                  onClick={this.handleOpenExplorer.bind(this, 'tx', data.id)}
                >
                  {data.id}
                  <SVGInline svg={externalLinkIcon} />
                </span>
              </div>
              <div className={styles.pendingTxnNote}>
                This transaction has been pending for too long. We recomend you
                cancel it.
                <span
                  role="presentation"
                  aria-hidden
                  className={styles.articleLink}
                  onClick={this.handleOpenSupportArticle}
                >
                  Read Why
                  <SVGInline svg={externalLinkIcon} />
                </span>
              </div>
              <div>
                <CancelTransactionButton
                  onClick={this.deletePendingTransaction}
                />
              </div>
            </div>
          </div>
          <SVGInline svg={arrow} className={arrowStyles} />
        </div>
      </div>
    );
  }
}
