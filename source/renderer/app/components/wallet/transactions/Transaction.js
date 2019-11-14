// @flow
import React, { Component, Fragment } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import { includes } from 'lodash';
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
import type { TransactionState } from '../../../api/transactions/types';
import { getNetworkExplorerUrl } from '../../../utils/network';
import { PENDING_TIME_LIMIT } from '../../../config/txnsConfig';
import CancelTransactionConfirmationDialog from './CancelTransactionConfirmationDialog';
import { ITN_BALANCE_CHECK } from '../../../../../common/types/environment.types';

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
  cancelPendingTxnNote: {
    id: 'wallet.transaction.pending.cancelPendingTxnNote',
    defaultMessage:
      '!!!This transaction has been pending for a long time. To release the funds used by this transaction, you can try canceling it.',
    description: 'Note to cancel a transaction that has been pending too long',
  },
  supportArticleLink: {
    id: 'wallet.transaction.pending.supportArticleLink',
    defaultMessage: '!!!Why should I cancel this transaction?',
    description: 'Link to support article for canceling a pending transaction',
  },
  supportArticleUrl: {
    id: 'wallet.transaction.pending.supportArticleUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360038113814',
    description: 'Url to support article for canceling a pending transaction',
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
  deletePendingTransaction: Function,
  state: TransactionState,
  isExpanded: boolean,
  isRestoreActive: boolean,
  isLastInList: boolean,
  formattedWalletAmount: Function,
  network: string,
  rawNetwork: string,
  onDetailsToggled: ?Function,
  onOpenExternalLink: ?Function,
  currentTimeFormat: string,
  walletId: string,
  isDeletingTransaction: boolean,
  currentLocale: string,
};

type State = {
  showConfirmationDialog: boolean,
};

export default class Transaction extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    showConfirmationDialog: false,
  };

  toggleDetails() {
    const { onDetailsToggled } = this.props;
    if (onDetailsToggled) onDetailsToggled();
  }

  handleOpenExplorer(type: string, param: string, e: Event) {
    const {
      onOpenExternalLink,
      network,
      rawNetwork,
      currentLocale,
    } = this.props;
    let queryStringPrefix = '';
    let localePrefix = '';
    let typeValue = type;

    if (network === ITN_BALANCE_CHECK) {
      queryStringPrefix = '?id=';
      localePrefix = `/${currentLocale.substr(0, 2)}`;
      if (type === 'tx') typeValue = 'transaction';
    }

    if (onOpenExternalLink) {
      e.stopPropagation();
      const link = `${getNetworkExplorerUrl(
        network,
        rawNetwork
      )}${localePrefix}/${typeValue}/${queryStringPrefix}${param}`;
      onOpenExternalLink(link);
    }
  }

  handleOpenSupportArticle = () => {
    const { intl } = this.context;
    const { onOpenExternalLink } = this.props;
    const supportArticleUrl = intl.formatMessage(messages.supportArticleUrl);
    if (!onOpenExternalLink) return null;
    return onOpenExternalLink(supportArticleUrl);
  };

  deletePendingTransaction = async () => {
    const { data, walletId } = this.props;
    const { id: transactionId, state } = data;
    if (state !== TransactionStates.PENDING) {
      return this.hideConfirmationDialog();
    }
    await this.props.deletePendingTransaction({
      walletId,
      transactionId,
    });
    return this.hideConfirmationDialog();
  };

  showConfirmationDialog = () => {
    this.setState({ showConfirmationDialog: true });
  };

  hideConfirmationDialog = () => {
    this.setState({ showConfirmationDialog: false });
  };

  getTimePending = (txnDate: Date): number => {
    // right now (milliseconds) minus txn created_at date (milliseconds)
    const NOW = moment().valueOf();
    const TXN_CREATED_AT = moment(txnDate).valueOf();
    return NOW - TXN_CREATED_AT;
  };

  hasExceededPendingTimeLimit = (): boolean => {
    const {
      data: { date },
      isRestoreActive,
      state,
    } = this.props;

    const isPendingTxn = state === TransactionStates.PENDING;
    if (!isPendingTxn || isRestoreActive || !date) return false;

    const TOTAL_TIME_PENDING = this.getTimePending(date);
    return TOTAL_TIME_PENDING > PENDING_TIME_LIMIT;
  };

  renderCancelPendingTxnContent = () => {
    const { intl } = this.context;
    const overPendingTimeLimit = this.hasExceededPendingTimeLimit();

    if (!overPendingTimeLimit) return null;

    return (
      <Fragment>
        <div className={styles.pendingTxnNote}>
          {intl.formatMessage(messages.cancelPendingTxnNote)}
          <span
            role="presentation"
            aria-hidden
            className={styles.articleLink}
            onClick={this.handleOpenSupportArticle}
          >
            {intl.formatMessage(messages.supportArticleLink)}
            <SVGInline svg={externalLinkIcon} />
          </span>
        </div>
        <div>
          <CancelTransactionButton onClick={this.showConfirmationDialog} />
        </div>
      </Fragment>
    );
  };

  renderTxnStateTag = () => {
    const { intl } = this.context;
    const { state } = this.props;
    const styleLabel = this.hasExceededPendingTimeLimit()
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
      isDeletingTransaction,
      currentTimeFormat,
    } = this.props;
    const { intl } = this.context;

    const { showConfirmationDialog } = this.state;

    const canOpenExplorer = onOpenExternalLink;

    const isPendingTransaction = state === TransactionStates.PENDING;

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

    const exceedsPendingTimeLimit = this.hasExceededPendingTimeLimit();

    const includesUnresolvedAddresses = addresses => includes(addresses, null);

    const fromAddresses = (addresses, transactionId) => {
      if (addresses.length > 0) {
        return includesUnresolvedAddresses(addresses) ? (
          <div className={styles.explorerLinkRow}>
            <span
              role="presentation"
              aria-hidden
              className={styles.explorerLink}
              onClick={this.handleOpenExplorer.bind(this, 'tx', transactionId)}
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
        ) : (
          addresses.map((address, addressIndex) => (
            <div
              // eslint-disable-next-line react/no-array-index-key
              key={`${data.id}-from-${address || ''}-${addressIndex}`}
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
          ))
        );
      }
      return <span>{intl.formatMessage(messages.noInputAddressesLabel)}</span>;
    };

    return (
      <Fragment>
        <div
          onClick={this.toggleDetails.bind(this)}
          className={componentStyles}
          role="presentation"
          aria-hidden
        >
          <div className={styles.toggler}>
            <TransactionTypeIcon
              exceedsPendingTimeLimit={exceedsPendingTimeLimit}
              iconType={iconType}
            />

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
                  {moment(data.date).format(currentTimeFormat)}
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
                {this.renderCancelPendingTxnContent()}
              </div>
            </div>
            <SVGInline svg={arrow} className={arrowStyles} />
          </div>
        </div>

        {showConfirmationDialog && (
          <CancelTransactionConfirmationDialog
            isSubmitting={isDeletingTransaction}
            onCancel={this.hideConfirmationDialog}
            onConfirm={this.deletePendingTransaction}
          />
        )}
      </Fragment>
    );
  }
}
