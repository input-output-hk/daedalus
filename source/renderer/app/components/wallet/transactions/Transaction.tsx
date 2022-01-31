import React, { Component, Fragment } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import { includes, get } from 'lodash';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import CancelTransactionButton from './CancelTransactionButton';
import { TransactionMetadataView } from './metadata/TransactionMetadataView';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './Transaction.scss' or its cor... Remove this comment to see the full error message
import styles from './Transaction.scss';
import TransactionTypeIcon from './TransactionTypeIcon';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/collaps... Remove this comment to see the full error message
import arrow from '../../../assets/images/collapse-arrow.inline.svg';
import {
  TransactionStates,
  TransactionTypes,
  WalletTransaction,
} from '../../../domains/WalletTransaction';
import WholeSelectionText from '../../widgets/WholeSelectionText';
import globalMessages from '../../../i18n/global-messages';
import type { TransactionState } from '../../../api/transactions/types';
import { PENDING_TIME_LIMIT } from '../../../config/txnsConfig';
import CancelTransactionConfirmationDialog from './CancelTransactionConfirmationDialog';
import type { AssetToken } from '../../../api/assets/types';
import Asset from '../../assets/Asset';
import AssetAmount from '../../assets/AssetAmount';
import { filterAssets } from '../../../utils/assets';
import { DiscreetWalletAmount } from '../../../features/discreet-mode';

/* eslint-disable consistent-return */
const messages = defineMessages({
  card: {
    id: 'wallet.transaction.type.card',
    defaultMessage: '!!!Card payment',
    description: 'Transaction type shown for credit card payments.',
  },
  type: {
    id: 'wallet.transaction.type',
    defaultMessage: '!!!{typeOfTransaction} transaction',
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
  transactionMetadata: {
    id: 'wallet.transaction.transactionMetadata',
    defaultMessage: '!!!Transaction Metadata',
    description: 'Transaction Metadata.',
  },
  transactionMetadataDescription: {
    id: 'wallet.transaction.transactionMetadataDescription',
    defaultMessage:
      'Transaction metadata is not moderated and may contain inappropriate content. Show unmoderated content.',
    description: '',
  },
  metadataLabel: {
    id: 'wallet.transaction.metadataLabel',
    defaultMessage: '!!!Transaction metadata',
    description: 'Transaction metadata label',
  },
  metadataDisclaimer: {
    id: 'wallet.transaction.metadataDisclaimer',
    defaultMessage:
      '!!!Transaction metadata is not moderated and may contain inappropriate content.',
    description: 'Transaction metadata disclaimer',
  },
  metadataConfirmationLabel: {
    id: 'wallet.transaction.metadataConfirmationLabel',
    defaultMessage: '!!!Show unmoderated content',
    description: 'Transaction metadata confirmation toggle',
  },
  conversionRate: {
    id: 'wallet.transaction.conversion.rate',
    defaultMessage: '!!!Conversion rate',
    description: 'Conversion rate.',
  },
  sent: {
    id: 'wallet.transaction.sent',
    defaultMessage: '!!!{transactionsType} sent',
    description: 'Label "{transactionsType} sent" for the transaction.',
  },
  received: {
    id: 'wallet.transaction.received',
    defaultMessage: '!!!{transactionsType} received',
    description: 'Label "{transactionsType} received" for the transaction.',
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
  fromRewards: {
    id: 'wallet.transaction.rewards.from',
    defaultMessage: '!!!From rewards',
    description: 'From rewards',
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
  receiverLabel: {
    id: 'wallet.transaction.receiverLabel',
    defaultMessage: '!!!Receiver',
    description: 'Receiver',
  },
  assetLabel: {
    id: 'wallet.transaction.assetLabel',
    defaultMessage: '!!!Token',
    description: 'Token label',
  },
  transactionFee: {
    id: 'wallet.transaction.transactionFee',
    defaultMessage: '!!!Transaction fee',
    description: 'Transaction fee',
  },
  deposit: {
    id: 'wallet.transaction.deposit',
    defaultMessage: '!!!Deposit',
    description: 'Deposit',
  },
  transactionAmount: {
    id: 'wallet.transaction.transactionAmount',
    defaultMessage: '!!!Transaction amount',
    description: 'Transaction amount.',
  },
  multipleTokens: {
    id: 'wallet.transaction.multipleTokens',
    defaultMessage: '!!!Multiple tokens',
    description: 'Multiple tokens.',
  },
  tokensSent: {
    id: 'wallet.transaction.tokensSent',
    defaultMessage: '!!!Tokens sent',
    description: 'Tokens sent.',
  },
  tokensReceived: {
    id: 'wallet.transaction.tokensReceived',
    defaultMessage: '!!!Tokens received',
    description: 'Tokens received.',
  },
  fetchingTokenData: {
    id: 'wallet.transaction.fetchingTokenData',
    defaultMessage: '!!!Fetching token data',
    description: '"Fetching token data..." message.',
  },
  cancelPendingTxnNote: {
    id: 'wallet.transaction.pending.cancelPendingTxnNote',
    defaultMessage:
      '!!!This transaction has been pending for a long time. To release the funds used by this transaction, you can try canceling it.',
    description: 'Note to cancel a transaction that has been pending too long',
  },
  cancelPendingTxnSupportArticle: {
    id: 'wallet.transaction.pending.cancelPendingTxnSupportArticle',
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
  cancelFailedTxnNote: {
    id: 'wallet.transaction.failed.cancelFailedTxnNote',
    defaultMessage:
      '!!!This transaction was submitted to the Cardano network, but it expired, so it failed. Transactions on the Cardano network have a ‘time to live’ attribute, which passed before the network processed the transaction. Please, remove it to release the funds (UTXOs) used by this transaction to use those funds in another transaction.',
    description: 'Note to cancel a transaction that has been failed',
  },
  cancelFailedTxnSupportArticle: {
    id: 'wallet.transaction.failed.cancelFailedTxnSupportArticle',
    defaultMessage: '!!!Why should I cancel failed transactions?',
    description: 'Link to support article for removing a failed transaction',
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
  [TransactionStates.FAILED]: {
    id: 'wallet.transaction.state.failed',
    defaultMessage: '!!!Transaction failed',
    description: 'Transaction state "failed"',
  },
});
const headerStateTranslations = defineMessages({
  [TransactionStates.OK]: {
    id: 'wallet.transaction.state.confirmedHeading',
    defaultMessage: '!!!Confirmed',
    description: 'Transaction state "confirmed"',
  },
  [TransactionStates.PENDING]: {
    id: 'wallet.transaction.state.pendingHeading',
    defaultMessage: '!!!Pending',
    description: 'Transaction state "pending"',
  },
  [TransactionStates.FAILED]: {
    id: 'wallet.transaction.state.failedHeading',
    defaultMessage: '!!!Failed',
    description: 'Transaction state "failed"',
  },
});
type Props = {
  data: WalletTransaction;
  deletePendingTransaction: (...args: Array<any>) => any;
  state: TransactionState;
  isExpanded: boolean;
  isRestoreActive: boolean;
  isLastInList: boolean;
  isShowingMetadata: boolean;
  formattedWalletAmount: (...args: Array<any>) => any;
  onDetailsToggled: ((...args: Array<any>) => any) | null | undefined;
  onOpenExternalLink: (...args: Array<any>) => any;
  onShowMetadata: () => void;
  getUrlByType: (...args: Array<any>) => any;
  currentTimeFormat: string;
  walletId: string;
  isDeletingTransaction: boolean;
  assetTokens: Array<AssetToken>;
  hasAssetsEnabled: boolean;
  isInternalAddress: (...args: Array<any>) => any;
  isLoadingAssets: boolean;
  onCopyAssetParam: (...args: Array<any>) => any;
};
type State = {
  showConfirmationDialog: boolean;
  showUnmoderatedMetadata: boolean;
};
export default class Transaction extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    showConfirmationDialog: false,
    showUnmoderatedMetadata: false,
  };

  componentDidUpdate(prevProps: Props, prevState: State) {
    // Tell parent components that meta data was toggled
    if (
      !prevState.showUnmoderatedMetadata &&
      this.state.showUnmoderatedMetadata &&
      this.props.onShowMetadata
    ) {
      this.props.onShowMetadata();
    }
  }

  toggleDetails() {
    const { onDetailsToggled } = this.props;
    if (onDetailsToggled) onDetailsToggled();
  }

  handleOpenSupportArticle = () => {
    const { intl } = this.context;
    const { onOpenExternalLink } = this.props;
    const supportArticleUrl = intl.formatMessage(messages.supportArticleUrl);
    return onOpenExternalLink(supportArticleUrl);
  };
  deletePendingTransaction = async () => {
    const { data, walletId } = this.props;
    const { id: transactionId, state } = data;

    if (
      state !== TransactionStates.PENDING &&
      state !== TransactionStates.FAILED
    ) {
      return this.hideConfirmationDialog();
    }

    await this.props.deletePendingTransaction({
      walletId,
      transactionId,
    });
    return this.hideConfirmationDialog();
  };
  showConfirmationDialog = () => {
    this.setState({
      showConfirmationDialog: true,
    });
  };
  hideConfirmationDialog = () => {
    this.setState({
      showConfirmationDialog: false,
    });
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
    const { data } = this.props;
    const { state } = data;
    const { intl } = this.context;
    const overPendingTimeLimit = this.hasExceededPendingTimeLimit();

    if (overPendingTimeLimit || state === TransactionStates.FAILED) {
      return (
        <Fragment>
          <div className={styles.pendingTxnNote}>
            {state === TransactionStates.PENDING
              ? intl.formatMessage(messages.cancelPendingTxnNote)
              : intl.formatMessage(messages.cancelFailedTxnNote)}
            <Link
              className={styles.articleLink}
              onClick={this.handleOpenSupportArticle}
              label={
                state === TransactionStates.PENDING
                  ? intl.formatMessage(messages.cancelPendingTxnSupportArticle)
                  : intl.formatMessage(messages.cancelFailedTxnSupportArticle)
              }
              underlineOnHover
              skin={LinkSkin}
            />
          </div>
          <div>
            <CancelTransactionButton
              state={state === TransactionStates.PENDING ? 'cancel' : 'remove'}
              onClick={
                state === TransactionStates.PENDING
                  ? this.showConfirmationDialog
                  : this.deletePendingTransaction
              }
            />
          </div>
        </Fragment>
      );
    }

    return null;
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

  get hasAssets(): boolean {
    return !!this.assetsList.length;
  }

  get assetsList(): Array<AssetToken> {
    const {
      assetTokens,
      data,
      isInternalAddress,
      hasAssetsEnabled,
    } = this.props;

    if (!hasAssetsEnabled) {
      return [];
    }

    return filterAssets(assetTokens, data.type, isInternalAddress);
  }

  includesUnresolvedAddresses = (addresses: Array<string | null | undefined>) =>
    includes(addresses, null);
  addressesList = (addresses: Array<string | null | undefined>): any => {
    const { intl } = this.context;
    const { onOpenExternalLink, getUrlByType, data } = this.props;
    const type = this.hasAssets ? data.type : null;

    if (addresses && addresses.length > 0) {
      const hasUnresolvedAddresses = this.includesUnresolvedAddresses(
        addresses
      );
      return type !== TransactionTypes.EXPEND && hasUnresolvedAddresses ? (
        <div className={styles.explorerLinkRow}>
          <Link
            className={styles.explorerLink}
            onClick={() => onOpenExternalLink(getUrlByType('tx', data.id))}
            label={intl.formatMessage(
              messages.unresolvedInputAddressesLinkLabel
            )}
            skin={LinkSkin}
          />
          <span>
            {intl.formatMessage(
              messages.unresolvedInputAddressesAdditionalLabel
            )}
          </span>
        </div>
      ) : (
        addresses.map((address, addressIndex) => (
          <div // eslint-disable-next-line react/no-array-index-key
            key={`${data.id}-from-${address || ''}-${addressIndex}`}
            className={styles.addressRow}
          >
            <Link
              onClick={() =>
                onOpenExternalLink(getUrlByType('address', address))
              }
              label={
                <WholeSelectionText className={styles.address} text={address} />
              }
              skin={LinkSkin}
            />
          </div>
        ))
      );
    }

    return <span>{intl.formatMessage(messages.noInputAddressesLabel)}</span>;
  };

  render() {
    const {
      data,
      isLastInList,
      isShowingMetadata,
      state,
      formattedWalletAmount,
      onOpenExternalLink,
      getUrlByType,
      isExpanded,
      isDeletingTransaction,
      currentTimeFormat,
      isLoadingAssets,
      onCopyAssetParam,
    } = this.props;
    const { intl } = this.context;
    const { showConfirmationDialog } = this.state;
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
      isExpanded ? styles.detailsExpanded : styles.detailsClosed,
    ]);
    const arrowStyles = classNames([
      styles.arrow,
      isExpanded ? styles.arrowExpanded : null,
    ]);
    const transactionsType = this.hasAssets
      ? intl.formatMessage(messages.multipleTokens)
      : intl.formatMessage(globalMessages.adaUnit);
    const typeOfTransaction = this.hasAssets
      ? intl.formatMessage(headerStateTranslations[state])
      : intl.formatMessage(globalMessages.adaUnit);

    const getIconType = (txState) => {
      switch (txState) {
        case TransactionStates.PENDING:
          return TransactionStates.PENDING;

        case TransactionStates.FAILED:
          return TransactionStates.FAILED;

        default:
          return data.type;
      }
    };

    const exceedsPendingTimeLimit = this.hasExceededPendingTimeLimit();
    const assetsSeparatorStyles = classNames([
      styles.assetsSeparator,
      isExpanded ? styles.expanded : null,
    ]);
    const assetsSeparatorBasicHeight = 27;
    const assetsSeparatorCalculatedHeight = this.assetsList.length
      ? assetsSeparatorBasicHeight * this.assetsList.length - 15
      : assetsSeparatorBasicHeight;
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
              iconType={getIconType(state)}
            />

            <div className={styles.togglerContent}>
              <div className={styles.header}>
                <div className={styles.title}>
                  {data.type === TransactionTypes.EXPEND
                    ? intl.formatMessage(messages.sent, {
                        transactionsType,
                      })
                    : intl.formatMessage(messages.received, {
                        transactionsType,
                      })}
                </div>
                {data.amount && (
                  <div className={styles.amount}>
                    <DiscreetWalletAmount
                      amount={data.amount}
                      withCurrency={false}
                    />
                    <span>{intl.formatMessage(globalMessages.adaUnit)}</span>
                  </div>
                )}
              </div>

              <div className={styles.details}>
                <div className={styles.type}>
                  {intl.formatMessage(messages.type, {
                    typeOfTransaction,
                  })}
                  ,{' '}
                  {moment(data.date)
                    .locale(intl.locale)
                    .format(currentTimeFormat)}
                </div>
                {this.renderTxnStateTag()}
              </div>
            </div>
          </div>

          {/* ==== Toggleable Transaction Details ==== */}
          <div className={contentStyles}>
            <div
              className={detailsStyles}
              onClick={(event) => event.stopPropagation()}
              role="presentation"
              aria-hidden
            >
              <div>
                <h2>{intl.formatMessage(messages.fromAddresses)}</h2>
                {this.addressesList(get(data, 'addresses.from', []))}
                {data.addresses.withdrawals.length ? (
                  <>
                    <h2>{intl.formatMessage(messages.fromRewards)}</h2>
                    {data.addresses.withdrawals.map((address, addressIndex) => (
                      <div // eslint-disable-next-line react/no-array-index-key
                        key={`${data.id}-to-${address}-${addressIndex}`}
                        className={styles.addressRow}
                      >
                        <Link
                          onClick={() =>
                            onOpenExternalLink(getUrlByType('address', address))
                          }
                          label={
                            <WholeSelectionText
                              className={styles.address}
                              text={address}
                            />
                          }
                          skin={LinkSkin}
                        />
                      </div>
                    ))}
                  </>
                ) : null}

                <h2>{intl.formatMessage(messages.toAddresses)}</h2>
                {this.addressesList(get(data, 'addresses.to', []))}

                {data.type === TransactionTypes.EXPEND && !data.fee.isZero() && (
                  <>
                    <h2>{intl.formatMessage(messages.transactionFee)}</h2>
                    <div className={styles.transactionFeeRow}>
                      <div className={styles.transactionFeeValue}>
                        {formattedWalletAmount(data.fee, false)}&nbsp;
                        <span>
                          {intl.formatMessage(globalMessages.adaUnit)}
                        </span>
                      </div>
                    </div>
                  </>
                )}

                {!data.deposit.isZero() && (
                  <>
                    <h2>{intl.formatMessage(messages.deposit)}</h2>
                    <div className={styles.depositRow}>
                      <div className={styles.depositValue}>
                        <DiscreetWalletAmount
                          amount={data.deposit}
                          withCurrency={false}
                        />
                        &nbsp;
                        <span>
                          {intl.formatMessage(globalMessages.adaUnit)}
                        </span>
                      </div>
                    </div>
                  </>
                )}

                {this.hasAssets && (
                  <>
                    <h2>
                      {data.type === TransactionTypes.EXPEND
                        ? intl.formatMessage(messages.tokensSent)
                        : intl.formatMessage(messages.tokensReceived)}
                    </h2>
                    {isLoadingAssets ? (
                      <div className={styles.assetContainer}>
                        <div
                          className={assetsSeparatorStyles}
                          style={{
                            height: '12px',
                          }}
                        />
                        <h3>
                          <span className={styles.fetchingTokenData}>
                            {intl.formatMessage(messages.fetchingTokenData)}
                          </span>
                        </h3>
                      </div>
                    ) : (
                      this.assetsList.map((asset, assetIndex) => (
                        <div // eslint-disable-next-line react/no-array-index-key
                          key={`${data.id}-to-${asset.policyId}-${assetIndex}`}
                          className={styles.assetContainer}
                        >
                          {assetIndex === 0 && (
                            <div
                              className={assetsSeparatorStyles}
                              style={{
                                height: `${assetsSeparatorCalculatedHeight}px`,
                              }}
                            />
                          )}
                          <h3>
                            <span>
                              {intl.formatMessage(messages.assetLabel)}
                              &nbsp;#{assetIndex + 1}
                            </span>
                            <Asset
                              asset={asset}
                              onCopyAssetParam={onCopyAssetParam}
                              className={styles.assetToken}
                            />
                          </h3>
                          {asset.quantity && (
                            <AssetAmount
                              amount={asset.quantity}
                              metadata={asset.metadata}
                              decimals={asset.decimals}
                              className={styles.assetAmount}
                            />
                          )}
                        </div>
                      ))
                    )}
                  </>
                )}

                <h2>{intl.formatMessage(messages.transactionId)}</h2>
                <div className={styles.transactionIdRow}>
                  <Link
                    onClick={() =>
                      onOpenExternalLink(getUrlByType('tx', data.id))
                    }
                    label={
                      <WholeSelectionText
                        className={styles.transactionId}
                        text={data.id}
                      />
                    }
                    skin={LinkSkin}
                  />
                </div>
                {this.renderCancelPendingTxnContent()}

                {data.metadata != null && (
                  <div className={styles.metadata}>
                    <h2>{intl.formatMessage(messages.metadataLabel)}</h2>
                    {data.metadata &&
                    (this.state.showUnmoderatedMetadata ||
                      isShowingMetadata) ? (
                      <TransactionMetadataView data={data.metadata} />
                    ) : (
                      <>
                        <p className={styles.metadataDisclaimer}>
                          {intl.formatMessage(messages.metadataDisclaimer)}
                        </p>
                        <Link
                          isUnderlined={false}
                          hasIconAfter={false}
                          underlineOnHover
                          label={intl.formatMessage(
                            messages.metadataConfirmationLabel
                          )}
                          onClick={(e) => {
                            e.preventDefault();
                            this.setState({
                              showUnmoderatedMetadata: true,
                            });
                          }}
                        />
                      </>
                    )}
                  </div>
                )}
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
