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
import { getSupportUrl } from '../../../../../common/utils/reporting';
import type {
  TransactionState,
  TransactionType,
} from '../../../api/transactions/types';
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
  selfTransfer: {
    id: 'wallet.transaction.type.selfTransfer',
    defaultMessage: '!!!Transfer within this wallet',
    description:
      'Title for a pure ADA transfer whose input and output addresses are verified as belonging to this wallet.',
  },
  voted: {
    id: 'wallet.transaction.voted',
    defaultMessage: '!!!Delegation Transaction',
    description:
      'Title for delegation transactions. The certificate may delegate stake, voting power, or both, so the title does not name either one.',
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
  netWalletChange: {
    id: 'wallet.transaction.netWalletChange',
    defaultMessage: '!!!Net wallet change',
    description:
      'Label for an amount representing the net ADA balance change of the wallet.',
  },
  amountTransferred: {
    id: 'wallet.transaction.amountTransferred',
    defaultMessage: '!!!Amount transferred',
    description:
      'Label for a known intended ADA payment amount, distinct from fees and change.',
  },
  amountUnavailable: {
    id: 'wallet.transaction.amountUnavailable',
    defaultMessage: '!!!Unavailable',
    description: 'Shown when a transaction amount is not known.',
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
  cancelExpiredTxnNote: {
    id: 'wallet.transaction.failed.cancelFailedTxnNote',
    defaultMessage:
      '!!!This transaction expired before the Cardano network confirmed it. Remove it to release its inputs for another transaction.',
    description: 'Note explaining an expired transaction',
  },
  cancelExpiredTxnSupportArticle: {
    id: 'wallet.transaction.failed.cancelFailedTxnSupportArticle',
    defaultMessage: '!!!Why should I remove an expired transaction?',
    description: 'Link to support information about expired transactions',
  },
  rejectedTxnNote: {
    id: 'wallet.transaction.failed.rejectedTxnNote',
    defaultMessage:
      '!!!The submission was rejected and is not expected to confirm. Review the transaction before trying again.',
    description: 'Note explaining a rejected transaction submission.',
  },
  submissionUnknownTxnNote: {
    id: 'wallet.transaction.submissionUnknown.note',
    defaultMessage:
      '!!!Daedalus could not determine whether this transaction reached the network. Check transaction history before retrying to avoid a duplicate payment.',
    description: 'Warning shown when the outcome of submission is unknown.',
  },
});
const stateTranslations = defineMessages({
  [TransactionStates.OK]: {
    id: 'wallet.transaction.state.confirmed',
    defaultMessage: '!!!Confirmed',
    description: 'Transaction state "confirmed".',
  },
  [TransactionStates.PENDING]: {
    id: 'wallet.transaction.state.pending',
    defaultMessage: '!!!Submitted · Awaiting confirmation',
    description: 'Transaction state "pending".',
  },
  [TransactionStates.EXPIRED]: {
    id: 'wallet.transaction.state.expired',
    defaultMessage: '!!!Expired',
    description: 'Transaction state "expired before confirmation".',
  },
  [TransactionStates.FAILED]: {
    id: 'wallet.transaction.state.failed',
    defaultMessage: '!!!Failed',
    description: 'Transaction state "submission rejected".',
  },
  [TransactionStates.SUBMISSION_UNKNOWN]: {
    id: 'wallet.transaction.state.submissionUnknown',
    defaultMessage: '!!!Submission status unknown',
    description:
      'Transaction state used when Daedalus cannot determine whether submission reached the network.',
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

  toggleDetails = () => {
    const { onDetailsToggled } = this.props;
    if (onDetailsToggled) onDetailsToggled();
  };

  handleOpenSupportArticle = () => {
    const { intl } = this.context;
    const { onOpenExternalLink } = this.props;
    return onOpenExternalLink(getSupportUrl(intl.locale));
  };
  deletePendingTransaction = async () => {
    const { data, walletId } = this.props;
    const { id: transactionId, state } = data;

    if (
      state !== TransactionStates.PENDING &&
      state !== TransactionStates.EXPIRED
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
    const { state } = this.props.data;
    const { intl } = this.context;
    const overPendingTimeLimit = this.hasExceededPendingTimeLimit();

    if (
      state === TransactionStates.FAILED ||
      state === TransactionStates.SUBMISSION_UNKNOWN
    ) {
      return (
        <div className={styles.pendingTxnNote}>
          {intl.formatMessage(
            state === TransactionStates.FAILED
              ? messages.rejectedTxnNote
              : messages.submissionUnknownTxnNote
          )}
        </div>
      );
    }

    if (overPendingTimeLimit || state === TransactionStates.EXPIRED) {
      const isPending = state === TransactionStates.PENDING;
      return (
        <Fragment>
          <div className={styles.pendingTxnNote}>
            {intl.formatMessage(
              isPending
                ? messages.cancelPendingTxnNote
                : messages.cancelExpiredTxnNote
            )}
            <Link
              className={styles.articleLink}
              onClick={this.handleOpenSupportArticle}
              label={intl.formatMessage(
                isPending
                  ? messages.cancelPendingTxnSupportArticle
                  : messages.cancelExpiredTxnSupportArticle
              )}
              underlineOnHover
              skin={LinkSkin}
            />
          </div>
          <div>
            <CancelTransactionButton
              state={isPending ? 'cancel' : 'remove'}
              onClick={
                isPending
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
    const stateStyles = {
      [TransactionStates.OK]: styles.inLedgerLabel,
      [TransactionStates.PENDING]: this.hasExceededPendingTimeLimit()
        ? styles.pendingWarningLabel
        : styles.pendingLabel,
      [TransactionStates.EXPIRED]: styles.expiredLabel,
      [TransactionStates.FAILED]: styles.failedLabel,
      [TransactionStates.SUBMISSION_UNKNOWN]: styles.submissionUnknownLabel,
    };
    return (
      <div className={stateStyles[state]} role="status">
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
    const hasTransferAmount = data.transferAmount != null;
    const displayedAmount = hasTransferAmount
      ? data.transferAmount
      : data.amount;
    const isAmountKnown =
      data.amountIsKnown !== false && displayedAmount != null;
    let amountSign = '';
    if (
      !hasTransferAmount &&
      displayedAmount &&
      displayedAmount.isGreaterThan(0)
    ) {
      if (data.type === TransactionTypes.INCOME) amountSign = '+';
      if (data.type === TransactionTypes.EXPEND) amountSign = '−';
    }

    const getIconType = (txState) => {
      switch (txState) {
        case TransactionStates.PENDING:
          return TransactionStates.PENDING;
        case TransactionStates.EXPIRED:
        case TransactionStates.FAILED:
          return TransactionStates.FAILED;
        default:
          return data.type;
      }
    };

    const getTitle = (txType: TransactionType): string => {
      if (data.isSelfTransfer === true) {
        return intl.formatMessage(messages.selfTransfer);
      }

      switch (txType) {
        case TransactionTypes.EXPEND:
          return intl.formatMessage(messages.sent, {
            transactionsType,
          });
        case TransactionTypes.VOTE:
          return intl.formatMessage(messages.voted);
        default:
          return intl.formatMessage(messages.received, {
            transactionsType,
          });
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
        <div className={componentStyles}>
          <div
            className={styles.toggler}
            onClick={this.toggleDetails}
            onKeyDown={(event) => {
              if (event.key === 'Enter' || event.key === ' ') {
                event.preventDefault();
                this.toggleDetails();
              }
            }}
            role="button"
            tabIndex={0}
            aria-expanded={isExpanded}
            aria-controls={`tx-details-${data.id}`}
          >
            <TransactionTypeIcon
              exceedsPendingTimeLimit={exceedsPendingTimeLimit}
              iconType={getIconType(state)}
            />

            <div className={styles.togglerContent}>
              <div className={styles.header}>
                <div className={styles.title}>{getTitle(data.type)}</div>
                <div className={styles.amount}>
                  <span className={styles.amountLabel}>
                    {intl.formatMessage(
                      hasTransferAmount
                        ? messages.amountTransferred
                        : messages.netWalletChange
                    )}
                  </span>
                  <span className={styles.amountValue}>
                    {isAmountKnown ? (
                      <>
                        {amountSign}
                        <DiscreetWalletAmount
                          amount={displayedAmount}
                          withCurrency={false}
                        />
                        <span>
                          {intl.formatMessage(globalMessages.adaUnit)}
                        </span>
                      </>
                    ) : (
                      intl.formatMessage(messages.amountUnavailable)
                    )}
                  </span>
                </div>
              </div>

              <div className={styles.details}>
                <div className={styles.type}>
                  {moment(data.date)
                    .locale(intl.locale)
                    .format(currentTimeFormat)}
                </div>
                {this.renderTxnStateTag()}
              </div>
            </div>
          </div>

          {/* ==== Toggleable Transaction Details ==== */}
          <div className={contentStyles} id={`tx-details-${data.id}`}>
            <div
              className={detailsStyles}
              onClick={(event) => event.stopPropagation()}
              role="presentation"
              aria-hidden={!isExpanded}
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
                    <div>
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
                    <div>
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
