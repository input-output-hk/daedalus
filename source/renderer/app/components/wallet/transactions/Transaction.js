// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
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
  noInputAddressesLabel: {
    id: 'wallet.transaction.noInputAddressesLabel',
    defaultMessage: '!!!No addresses',
    description: 'Input Addresses label.',
  },
  unresolvedInputAddressesLinkLabel: {
    id: 'wallet.transaction.unresolvedInputAddressesLinkLabel',
    defaultMessage: '!!!Open this transaction in Cardano explorer',
    description: 'Unresolved Input Addresses link label.',
  },
  unresolvedInputAddressesAdditionalLabel: {
    id: 'wallet.transaction.unresolvedInputAddressesAdditionalLabel',
    defaultMessage: '!!!to see these addresses.',
    description: 'Unresolved Input Addresses additional label.',
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
  state: TransactionState,
  isExpanded: boolean,
  isRestoreActive: boolean,
  isLastInList: boolean,
  formattedWalletAmount: Function,
  network: string,
  onDetailsToggled: ?Function,
  onOpenExternalLink: ?Function,
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

  render() {
    const {
      data,
      isLastInList,
      state,
      formattedWalletAmount,
      onOpenExternalLink,
      isRestoreActive,
      isExpanded,
    } = this.props;
    const { intl } = this.context;

    const canOpenExplorer = onOpenExternalLink;

    const isPendingTransaction = state === TransactionStates.PENDING;

    const transactionState = isPendingTransaction
      ? TransactionStates.PENDING
      : state;

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

    const transactionStateTag = () => {
      if (isRestoreActive) return;

      return (
        <div className={styles[`${transactionState}Label`]}>
          {intl.formatMessage(stateTranslations[transactionState])}
        </div>
      );
    };

    const fromAddresses = (addresses, transactionId) => {
      if (addresses.length) {
        return addresses.map((address, addressIndex) =>
          address ? (
            <div
              // eslint-disable-next-line react/no-array-index-key
              key={`${data.id}-from-${address}-${addressIndex}`}
              className={styles.addressRow}
            >
              <span
                role="presentation"
                aria-hidden
                className={styles.address}
                onClick={this.handleOpenExplorer.bind(this, 'address', address)}
              >
                {address}
                <SVGInline svg={externalLinkIcon} />
              </span>
            </div>
          ) : (
            <div className={styles.explorerLinkRow}>
              <span
                role="presentation"
                aria-hidden
                className={styles.explorerLink}
                onClick={this.handleOpenExplorer.bind(
                  this,
                  'tx',
                  transactionId
                )}
              >
                {intl.formatMessage(messages.unresolvedInputAddressesLinkLabel)}
                <SVGInline svg={externalLinkIcon} />
              </span>
              <span>
                {intl.formatMessage(
                  messages.unresolvedInputAddressesAdditionalLabel
                )}
              </span>
            </div>
          )
        );
      }
      return <span>{intl.formatMessage(messages.noInputAddressesLabel)}</span>;
    };

    return (
      <div
        onClick={this.toggleDetails.bind(this)}
        className={componentStyles}
        role="presentation"
        aria-hidden
      >
        <div className={styles.toggler}>
          <TransactionTypeIcon iconType={data.type} />

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
              {transactionStateTag()}
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

              {fromAddresses(data.addresses.from, data.id)}

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
            </div>
          </div>
          <SVGInline svg={arrow} className={arrowStyles} />
        </div>
      </div>
    );
  }
}
