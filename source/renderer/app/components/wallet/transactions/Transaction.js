'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
// @ts-nocheck
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const moment_1 = __importDefault(require('moment'));
const lodash_1 = require('lodash');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const CancelTransactionButton_1 = __importDefault(
  require('./CancelTransactionButton')
);
const TransactionMetadataView_1 = require('./metadata/TransactionMetadataView');
const Transaction_scss_1 = __importDefault(require('./Transaction.scss'));
const TransactionTypeIcon_1 = __importDefault(require('./TransactionTypeIcon'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/collaps... Remove this comment to see the full error message
const collapse_arrow_inline_svg_1 = __importDefault(
  require('../../../assets/images/collapse-arrow.inline.svg')
);
const WalletTransaction_1 = require('../../../domains/WalletTransaction');
const WholeSelectionText_1 = __importDefault(
  require('../../widgets/WholeSelectionText')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const txnsConfig_1 = require('../../../config/txnsConfig');
const CancelTransactionConfirmationDialog_1 = __importDefault(
  require('./CancelTransactionConfirmationDialog')
);
const Asset_1 = __importDefault(require('../../assets/Asset'));
const AssetAmount_1 = __importDefault(require('../../assets/AssetAmount'));
const assets_1 = require('../../../utils/assets');
const discreet_mode_1 = require('../../../features/discreet-mode');
/* eslint-disable consistent-return */
const messages = (0, react_intl_1.defineMessages)({
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
const stateTranslations = (0, react_intl_1.defineMessages)({
  [WalletTransaction_1.TransactionStates.OK]: {
    id: 'wallet.transaction.state.confirmed',
    defaultMessage: '!!!Transaction confirmed',
    description: 'Transaction state "confirmed"',
  },
  [WalletTransaction_1.TransactionStates.PENDING]: {
    id: 'wallet.transaction.state.pending',
    defaultMessage: '!!!Transaction pending',
    description: 'Transaction state "pending"',
  },
  [WalletTransaction_1.TransactionStates.FAILED]: {
    id: 'wallet.transaction.state.failed',
    defaultMessage: '!!!Transaction failed',
    description: 'Transaction state "failed"',
  },
});
const headerStateTranslations = (0, react_intl_1.defineMessages)({
  [WalletTransaction_1.TransactionStates.OK]: {
    id: 'wallet.transaction.state.confirmedHeading',
    defaultMessage: '!!!Confirmed',
    description: 'Transaction state "confirmed"',
  },
  [WalletTransaction_1.TransactionStates.PENDING]: {
    id: 'wallet.transaction.state.pendingHeading',
    defaultMessage: '!!!Pending',
    description: 'Transaction state "pending"',
  },
  [WalletTransaction_1.TransactionStates.FAILED]: {
    id: 'wallet.transaction.state.failedHeading',
    defaultMessage: '!!!Failed',
    description: 'Transaction state "failed"',
  },
});
class Transaction extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    showConfirmationDialog: false,
    showUnmoderatedMetadata: false,
  };
  componentDidUpdate(prevProps, prevState) {
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
      state !== WalletTransaction_1.TransactionStates.PENDING &&
      state !== WalletTransaction_1.TransactionStates.FAILED
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
  getTimePending = (txnDate) => {
    // right now (milliseconds) minus txn created_at date (milliseconds)
    const NOW = (0, moment_1.default)().valueOf();
    const TXN_CREATED_AT = (0, moment_1.default)(txnDate).valueOf();
    return NOW - TXN_CREATED_AT;
  };
  hasExceededPendingTimeLimit = () => {
    const {
      data: { date },
      isRestoreActive,
      state,
    } = this.props;
    const isPendingTxn =
      state === WalletTransaction_1.TransactionStates.PENDING;
    if (!isPendingTxn || isRestoreActive || !date) return false;
    const TOTAL_TIME_PENDING = this.getTimePending(date);
    return TOTAL_TIME_PENDING > txnsConfig_1.PENDING_TIME_LIMIT;
  };
  renderCancelPendingTxnContent = () => {
    const { data } = this.props;
    const { state } = data;
    const { intl } = this.context;
    const overPendingTimeLimit = this.hasExceededPendingTimeLimit();
    if (
      overPendingTimeLimit ||
      state === WalletTransaction_1.TransactionStates.FAILED
    ) {
      return react_1.default.createElement(
        react_1.Fragment,
        null,
        react_1.default.createElement(
          'div',
          { className: Transaction_scss_1.default.pendingTxnNote },
          state === WalletTransaction_1.TransactionStates.PENDING
            ? intl.formatMessage(messages.cancelPendingTxnNote)
            : intl.formatMessage(messages.cancelFailedTxnNote),
          react_1.default.createElement(Link_1.Link, {
            className: Transaction_scss_1.default.articleLink,
            onClick: this.handleOpenSupportArticle,
            label:
              state === WalletTransaction_1.TransactionStates.PENDING
                ? intl.formatMessage(messages.cancelPendingTxnSupportArticle)
                : intl.formatMessage(messages.cancelFailedTxnSupportArticle),
            underlineOnHover: true,
            skin: LinkSkin_1.LinkSkin,
          })
        ),
        react_1.default.createElement(
          'div',
          null,
          react_1.default.createElement(CancelTransactionButton_1.default, {
            state:
              state === WalletTransaction_1.TransactionStates.PENDING
                ? 'cancel'
                : 'remove',
            onClick:
              state === WalletTransaction_1.TransactionStates.PENDING
                ? this.showConfirmationDialog
                : this.deletePendingTransaction,
          })
        )
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
    return react_1.default.createElement(
      'div',
      { className: Transaction_scss_1.default[styleLabel] },
      intl.formatMessage(stateTranslations[state])
    );
  };
  get hasAssets() {
    return !!this.assetsList.length;
  }
  get assetsList() {
    const {
      assetTokens,
      data,
      isInternalAddress,
      hasAssetsEnabled,
    } = this.props;
    if (!hasAssetsEnabled) {
      return [];
    }
    return (0, assets_1.filterAssets)(
      assetTokens,
      data.type,
      isInternalAddress
    );
  }
  includesUnresolvedAddresses = (addresses) =>
    (0, lodash_1.includes)(addresses, null);
  addressesList = (addresses) => {
    const { intl } = this.context;
    const { onOpenExternalLink, getUrlByType, data } = this.props;
    const type = this.hasAssets ? data.type : null;
    if (addresses && addresses.length > 0) {
      const hasUnresolvedAddresses = this.includesUnresolvedAddresses(
        addresses
      );
      return type !== WalletTransaction_1.TransactionTypes.EXPEND &&
        hasUnresolvedAddresses
        ? react_1.default.createElement(
            'div',
            { className: Transaction_scss_1.default.explorerLinkRow },
            react_1.default.createElement(Link_1.Link, {
              className: Transaction_scss_1.default.explorerLink,
              onClick: () => onOpenExternalLink(getUrlByType('tx', data.id)),
              label: intl.formatMessage(
                messages.unresolvedInputAddressesLinkLabel
              ),
              skin: LinkSkin_1.LinkSkin,
            }),
            react_1.default.createElement(
              'span',
              null,
              intl.formatMessage(
                messages.unresolvedInputAddressesAdditionalLabel
              )
            )
          )
        : addresses.map((address, addressIndex) =>
            react_1.default.createElement(
              'div',
              {
                key: `${data.id}-from-${address || ''}-${addressIndex}`,
                className: Transaction_scss_1.default.addressRow,
              },
              react_1.default.createElement(Link_1.Link, {
                onClick: () =>
                  onOpenExternalLink(getUrlByType('address', address)),
                label: react_1.default.createElement(
                  WholeSelectionText_1.default,
                  {
                    className: Transaction_scss_1.default.address,
                    text: address,
                  }
                ),
                skin: LinkSkin_1.LinkSkin,
              })
            )
          );
    }
    return react_1.default.createElement(
      'span',
      null,
      intl.formatMessage(messages.noInputAddressesLabel)
    );
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
    const componentStyles = (0, classnames_1.default)([
      Transaction_scss_1.default.component,
      isExpanded ? 'Transaction_expanded' : null,
    ]);
    const contentStyles = (0, classnames_1.default)([
      Transaction_scss_1.default.content,
      isLastInList ? Transaction_scss_1.default.last : null,
      isExpanded ? Transaction_scss_1.default.contentExpanded : null,
    ]);
    const detailsStyles = (0, classnames_1.default)([
      Transaction_scss_1.default.details,
      isExpanded
        ? Transaction_scss_1.default.detailsExpanded
        : Transaction_scss_1.default.detailsClosed,
    ]);
    const arrowStyles = (0, classnames_1.default)([
      Transaction_scss_1.default.arrow,
      isExpanded ? Transaction_scss_1.default.arrowExpanded : null,
    ]);
    const transactionsType = this.hasAssets
      ? intl.formatMessage(messages.multipleTokens)
      : intl.formatMessage(global_messages_1.default.adaUnit);
    const typeOfTransaction = this.hasAssets
      ? intl.formatMessage(headerStateTranslations[state])
      : intl.formatMessage(global_messages_1.default.adaUnit);
    const getIconType = (txState) => {
      switch (txState) {
        case WalletTransaction_1.TransactionStates.PENDING:
          return WalletTransaction_1.TransactionStates.PENDING;
        case WalletTransaction_1.TransactionStates.FAILED:
          return WalletTransaction_1.TransactionStates.FAILED;
        default:
          return data.type;
      }
    };
    const exceedsPendingTimeLimit = this.hasExceededPendingTimeLimit();
    const assetsSeparatorStyles = (0, classnames_1.default)([
      Transaction_scss_1.default.assetsSeparator,
      isExpanded ? Transaction_scss_1.default.expanded : null,
    ]);
    const assetsSeparatorBasicHeight = 27;
    const assetsSeparatorCalculatedHeight = this.assetsList.length
      ? assetsSeparatorBasicHeight * this.assetsList.length - 15
      : assetsSeparatorBasicHeight;
    return react_1.default.createElement(
      react_1.Fragment,
      null,
      react_1.default.createElement(
        'div',
        {
          onClick: this.toggleDetails.bind(this),
          className: componentStyles,
          role: 'presentation',
          'aria-hidden': true,
        },
        react_1.default.createElement(
          'div',
          { className: Transaction_scss_1.default.toggler },
          react_1.default.createElement(TransactionTypeIcon_1.default, {
            exceedsPendingTimeLimit: exceedsPendingTimeLimit,
            iconType: getIconType(state),
          }),
          react_1.default.createElement(
            'div',
            { className: Transaction_scss_1.default.togglerContent },
            react_1.default.createElement(
              'div',
              { className: Transaction_scss_1.default.header },
              react_1.default.createElement(
                'div',
                { className: Transaction_scss_1.default.title },
                data.type === WalletTransaction_1.TransactionTypes.EXPEND
                  ? intl.formatMessage(messages.sent, {
                      transactionsType,
                    })
                  : intl.formatMessage(messages.received, {
                      transactionsType,
                    })
              ),
              data.amount &&
                react_1.default.createElement(
                  'div',
                  { className: Transaction_scss_1.default.amount },
                  react_1.default.createElement(
                    discreet_mode_1.DiscreetWalletAmount,
                    { amount: data.amount, withCurrency: false }
                  ),
                  react_1.default.createElement(
                    'span',
                    null,
                    intl.formatMessage(global_messages_1.default.adaUnit)
                  )
                )
            ),
            react_1.default.createElement(
              'div',
              { className: Transaction_scss_1.default.details },
              react_1.default.createElement(
                'div',
                { className: Transaction_scss_1.default.type },
                intl.formatMessage(messages.type, {
                  typeOfTransaction,
                }),
                ',',
                ' ',
                (0, moment_1.default)(data.date)
                  .locale(intl.locale)
                  .format(currentTimeFormat)
              ),
              this.renderTxnStateTag()
            )
          )
        ),
        react_1.default.createElement(
          'div',
          { className: contentStyles },
          react_1.default.createElement(
            'div',
            {
              className: detailsStyles,
              onClick: (event) => event.stopPropagation(),
              role: 'presentation',
              'aria-hidden': true,
            },
            react_1.default.createElement(
              'div',
              null,
              react_1.default.createElement(
                'h2',
                null,
                intl.formatMessage(messages.fromAddresses)
              ),
              this.addressesList((0, lodash_1.get)(data, 'addresses.from', [])),
              data.addresses.withdrawals.length
                ? react_1.default.createElement(
                    react_1.default.Fragment,
                    null,
                    react_1.default.createElement(
                      'h2',
                      null,
                      intl.formatMessage(messages.fromRewards)
                    ),
                    data.addresses.withdrawals.map((address, addressIndex) =>
                      react_1.default.createElement(
                        'div',
                        {
                          key: `${data.id}-to-${address}-${addressIndex}`,
                          className: Transaction_scss_1.default.addressRow,
                        },
                        react_1.default.createElement(Link_1.Link, {
                          onClick: () =>
                            onOpenExternalLink(
                              getUrlByType('address', address)
                            ),
                          label: react_1.default.createElement(
                            WholeSelectionText_1.default,
                            {
                              className: Transaction_scss_1.default.address,
                              text: address,
                            }
                          ),
                          skin: LinkSkin_1.LinkSkin,
                        })
                      )
                    )
                  )
                : null,
              react_1.default.createElement(
                'h2',
                null,
                intl.formatMessage(messages.toAddresses)
              ),
              this.addressesList((0, lodash_1.get)(data, 'addresses.to', [])),
              data.type === WalletTransaction_1.TransactionTypes.EXPEND &&
                !data.fee.isZero() &&
                react_1.default.createElement(
                  react_1.default.Fragment,
                  null,
                  react_1.default.createElement(
                    'h2',
                    null,
                    intl.formatMessage(messages.transactionFee)
                  ),
                  react_1.default.createElement(
                    'div',
                    null,
                    react_1.default.createElement(
                      'div',
                      {
                        className:
                          Transaction_scss_1.default.transactionFeeValue,
                      },
                      formattedWalletAmount(data.fee, false),
                      '\u00A0',
                      react_1.default.createElement(
                        'span',
                        null,
                        intl.formatMessage(global_messages_1.default.adaUnit)
                      )
                    )
                  )
                ),
              !data.deposit.isZero() &&
                react_1.default.createElement(
                  react_1.default.Fragment,
                  null,
                  react_1.default.createElement(
                    'h2',
                    null,
                    intl.formatMessage(messages.deposit)
                  ),
                  react_1.default.createElement(
                    'div',
                    null,
                    react_1.default.createElement(
                      'div',
                      { className: Transaction_scss_1.default.depositValue },
                      react_1.default.createElement(
                        discreet_mode_1.DiscreetWalletAmount,
                        { amount: data.deposit, withCurrency: false }
                      ),
                      '\u00A0',
                      react_1.default.createElement(
                        'span',
                        null,
                        intl.formatMessage(global_messages_1.default.adaUnit)
                      )
                    )
                  )
                ),
              this.hasAssets &&
                react_1.default.createElement(
                  react_1.default.Fragment,
                  null,
                  react_1.default.createElement(
                    'h2',
                    null,
                    data.type === WalletTransaction_1.TransactionTypes.EXPEND
                      ? intl.formatMessage(messages.tokensSent)
                      : intl.formatMessage(messages.tokensReceived)
                  ),
                  isLoadingAssets
                    ? react_1.default.createElement(
                        'div',
                        {
                          className: Transaction_scss_1.default.assetContainer,
                        },
                        react_1.default.createElement('div', {
                          className: assetsSeparatorStyles,
                          style: {
                            height: '12px',
                          },
                        }),
                        react_1.default.createElement(
                          'h3',
                          null,
                          react_1.default.createElement(
                            'span',
                            {
                              className:
                                Transaction_scss_1.default.fetchingTokenData,
                            },
                            intl.formatMessage(messages.fetchingTokenData)
                          )
                        )
                      )
                    : this.assetsList.map((asset, assetIndex) =>
                        react_1.default.createElement(
                          'div',
                          {
                            key: `${data.id}-to-${asset.policyId}-${assetIndex}`,
                            className:
                              Transaction_scss_1.default.assetContainer,
                          },
                          assetIndex === 0 &&
                            react_1.default.createElement('div', {
                              className: assetsSeparatorStyles,
                              style: {
                                height: `${assetsSeparatorCalculatedHeight}px`,
                              },
                            }),
                          react_1.default.createElement(
                            'h3',
                            null,
                            react_1.default.createElement(
                              'span',
                              null,
                              intl.formatMessage(messages.assetLabel),
                              '\u00A0#',
                              assetIndex + 1
                            ),
                            react_1.default.createElement(Asset_1.default, {
                              asset: asset,
                              onCopyAssetParam: onCopyAssetParam,
                              className: Transaction_scss_1.default.assetToken,
                            })
                          ),
                          asset.quantity &&
                            react_1.default.createElement(
                              AssetAmount_1.default,
                              {
                                amount: asset.quantity,
                                metadata: asset.metadata,
                                decimals: asset.decimals,
                                className:
                                  Transaction_scss_1.default.assetAmount,
                              }
                            )
                        )
                      )
                ),
              react_1.default.createElement(
                'h2',
                null,
                intl.formatMessage(messages.transactionId)
              ),
              react_1.default.createElement(
                'div',
                { className: Transaction_scss_1.default.transactionIdRow },
                react_1.default.createElement(Link_1.Link, {
                  onClick: () =>
                    onOpenExternalLink(getUrlByType('tx', data.id)),
                  label: react_1.default.createElement(
                    WholeSelectionText_1.default,
                    {
                      className: Transaction_scss_1.default.transactionId,
                      text: data.id,
                    }
                  ),
                  skin: LinkSkin_1.LinkSkin,
                })
              ),
              this.renderCancelPendingTxnContent(),
              data.metadata != null &&
                react_1.default.createElement(
                  'div',
                  { className: Transaction_scss_1.default.metadata },
                  react_1.default.createElement(
                    'h2',
                    null,
                    intl.formatMessage(messages.metadataLabel)
                  ),
                  data.metadata &&
                    (this.state.showUnmoderatedMetadata || isShowingMetadata)
                    ? react_1.default.createElement(
                        TransactionMetadataView_1.TransactionMetadataView,
                        { data: data.metadata }
                      )
                    : react_1.default.createElement(
                        react_1.default.Fragment,
                        null,
                        react_1.default.createElement(
                          'p',
                          {
                            className:
                              Transaction_scss_1.default.metadataDisclaimer,
                          },
                          intl.formatMessage(messages.metadataDisclaimer)
                        ),
                        react_1.default.createElement(Link_1.Link, {
                          isUnderlined: false,
                          hasIconAfter: false,
                          underlineOnHover: true,
                          label: intl.formatMessage(
                            messages.metadataConfirmationLabel
                          ),
                          onClick: (e) => {
                            e.preventDefault();
                            this.setState({
                              showUnmoderatedMetadata: true,
                            });
                          },
                        })
                      )
                )
            )
          ),
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: collapse_arrow_inline_svg_1.default,
            className: arrowStyles,
          })
        )
      ),
      showConfirmationDialog &&
        react_1.default.createElement(
          CancelTransactionConfirmationDialog_1.default,
          {
            isSubmitting: isDeletingTransaction,
            onCancel: this.hideConfirmationDialog,
            onConfirm: this.deletePendingTransaction,
          }
        )
    );
  }
}
exports.default = Transaction;
//# sourceMappingURL=Transaction.js.map
