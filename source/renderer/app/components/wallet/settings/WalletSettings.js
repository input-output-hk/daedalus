// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import moment from 'moment';
import LocalizableError from '../../../i18n/LocalizableError';
import BorderedBox from '../../widgets/BorderedBox';
import InlineEditingInput from '../../widgets/forms/InlineEditingInput';
import ReadOnlyInput from '../../widgets/forms/ReadOnlyInput';
import DeleteWalletButton from './DeleteWalletButton';
import DeleteWalletConfirmationDialog from './DeleteWalletConfirmationDialog';
import ChangeSpendingPasswordDialog from './ChangeSpendingPasswordDialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletSettings.scss';
import WalletRecoveryPhrase from './WalletRecoveryPhrase';
import ResyncWalletButton from './ResyncWalletButton';

export const messages = defineMessages({
  assuranceLevelLabel: {
    id: 'wallet.settings.assurance',
    defaultMessage: '!!!Transaction assurance security level',
    description:
      'Label for the "Transaction assurance security level" dropdown.',
  },
  deleteWalletHeader: {
    id: 'wallet.settings.deleteWallet.header',
    defaultMessage: '!!!Delete wallet',
    description: 'Delete wallet header on the wallet settings page.',
  },
  deleteWalletWarning1: {
    id: 'wallet.settings.deleteWallet.warning1',
    defaultMessage:
      '!!!Once you delete this wallet it will be removed from the Daedalus interface and you will lose access to any remaining funds in the wallet. The only way to regain access after deletion is by restoring it using your wallet recovery phrase.',
    description: 'Delete wallet warning explaining the consequences.',
  },
  deleteWalletWarning2: {
    id: 'wallet.settings.deleteWallet.warning2',
    defaultMessage:
      '!!!You may wish to verify your recovery phrase before deletion to ensure that you can restore this wallet in the future, if desired.',
    description: 'Delete wallet warning explaining the consequences.',
  },
  resyncWalletHeader: {
    id: 'wallet.settings.resyncWallet.header',
    defaultMessage: '!!!Resync wallet with the blockchain',
    description: 'Resync wallet header on the wallet settings page.',
  },
  resyncWalletDescription: {
    id: 'wallet.settings.resyncWallet.description',
    defaultMessage:
      '!!!If you are experiencing issues with your wallet, or think you have an incorrect balance or transaction history, you can delete the local data stored by Daedalus and resync with the blockchain.',
    description: 'Resync wallet description.',
  },
  name: {
    id: 'wallet.settings.name.label',
    defaultMessage: '!!!Name',
    description: 'Label for the "Name" text input on the wallet settings page.',
  },
  passwordLabel: {
    id: 'wallet.settings.password',
    defaultMessage: '!!!Password',
    description: 'Label for the "Password" field.',
  },
  passwordLastUpdated: {
    id: 'wallet.settings.passwordLastUpdated',
    defaultMessage: '!!!Last updated',
    description: 'Last updated X time ago message.',
  },
});

type Props = {
  walletName: string,
  creationDate: Date,
  spendingPasswordUpdateDate: ?Date,
  error?: ?LocalizableError,
  openDialogAction: Function,
  isDialogOpen: Function,
  onFieldValueChange: Function,
  onStartEditing: Function,
  onStopEditing: Function,
  onCancelEditing: Function,
  onResyncWallet: Function,
  nameValidator: Function,
  activeField: ?string,
  isSubmitting: boolean,
  isForcedWalletResyncStarting: boolean,
  isIncentivizedTestnet: boolean,
  isInvalid: boolean,
  isLegacy: boolean,
  lastUpdatedField: ?string,
  changeSpendingPasswordDialog: Node,
  deleteWalletDialogContainer: Node,
  walletRecoveryPhraseStep1Container: Node,
  walletRecoveryPhraseStep2Container: Node,
  walletRecoveryPhraseStep3Container: Node,
  walletRecoveryPhraseStep4Container: Node,
  recoveryPhraseVerificationDate: ?Date,
  recoveryPhraseVerificationStatus: string,
  recoveryPhraseVerificationStatusType: string,
  locale: string,
};

type State = {
  isFormBlocked: boolean,
};

@observer
export default class WalletSettings extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isFormBlocked: false,
  };

  componentDidUpdate() {
    const { isDialogOpen } = this.props;
    const { isFormBlocked } = this.state;
    // Set "name" input to active and "unblock form" on Dialog close
    if (
      !isDialogOpen(DeleteWalletConfirmationDialog) &&
      !isDialogOpen(ChangeSpendingPasswordDialog) &&
      isFormBlocked
    ) {
      this.unblockForm();
    }
  }

  componentWillUnmount() {
    // This call is used to prevent display of old successfully-updated messages
    this.props.onCancelEditing();
  }

  onBlockForm = () => {
    this.setState({ isFormBlocked: true });
  };

  unblockForm = () => {
    this.setState({ isFormBlocked: false });
  };

  render() {
    const { intl } = this.context;
    const {
      walletName,
      creationDate,
      spendingPasswordUpdateDate,
      error,
      openDialogAction,
      isDialogOpen,
      onFieldValueChange,
      onStartEditing,
      onStopEditing,
      onCancelEditing,
      onResyncWallet,
      nameValidator,
      activeField,
      isSubmitting,
      isForcedWalletResyncStarting,
      isIncentivizedTestnet,
      isInvalid,
      isLegacy,
      lastUpdatedField,
      changeSpendingPasswordDialog,
      deleteWalletDialogContainer,
      walletRecoveryPhraseStep1Container,
      walletRecoveryPhraseStep2Container,
      walletRecoveryPhraseStep3Container,
      walletRecoveryPhraseStep4Container,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
      locale,
    } = this.props;
    const { isFormBlocked } = this.state;

    // Set Japanese locale to moment. Default is en-US
    if (locale === 'ja-JP') {
      moment.locale('ja');
    } else {
      moment.locale('en-us');
    }

    if (isLegacy) {
      const deleteWalletBoxStyles = classNames([
        styles.deleteWalletBox,
        styles.legacyWallet,
      ]);
      const resyncWalletBoxStyles = classNames([
        styles.resyncWalletBox,
        styles.legacyWallet,
      ]);
      return (
        <div className={styles.component}>
          <BorderedBox className={resyncWalletBoxStyles}>
            <span>{intl.formatMessage(messages.resyncWalletHeader)}</span>
            <div className={styles.contentBox}>
              <p>{intl.formatMessage(messages.resyncWalletDescription)}</p>
              <ResyncWalletButton
                isSubmitting={isForcedWalletResyncStarting}
                onClick={onResyncWallet}
              />
            </div>
          </BorderedBox>

          <BorderedBox className={deleteWalletBoxStyles}>
            <span>{intl.formatMessage(messages.deleteWalletHeader)}</span>
            <div className={styles.contentBox}>
              <div>
                <p>{intl.formatMessage(messages.deleteWalletWarning1)}</p>
                <p>{intl.formatMessage(messages.deleteWalletWarning2)}</p>
              </div>
              <DeleteWalletButton
                onClick={() =>
                  openDialogAction({
                    dialog: DeleteWalletConfirmationDialog,
                  })
                }
              />
            </div>
          </BorderedBox>

          {isDialogOpen(DeleteWalletConfirmationDialog)
            ? deleteWalletDialogContainer
            : false}
        </div>
      );
    }

    return (
      <div className={styles.component}>
        <BorderedBox>
          <InlineEditingInput
            className="walletName"
            inputFieldLabel={intl.formatMessage(messages.name)}
            inputFieldValue={walletName}
            maxLength={40}
            isActive={!isFormBlocked && activeField === 'name'}
            onStartEditing={() => onStartEditing('name')}
            onStopEditing={onStopEditing}
            onCancelEditing={onCancelEditing}
            onSubmit={value => onFieldValueChange('name', value)}
            isValid={nameValidator}
            validationErrorMessage={intl.formatMessage(
              globalMessages.invalidWalletName
            )}
            successfullyUpdated={
              !isSubmitting && lastUpdatedField === 'name' && !isInvalid
            }
            inputBlocked={isFormBlocked}
          />

          <ReadOnlyInput
            label={intl.formatMessage(messages.passwordLabel)}
            value={intl.formatMessage(messages.passwordLastUpdated, {
              lastUpdated: moment(spendingPasswordUpdateDate)
                .locale(this.context.intl.locale)
                .fromNow(),
            })}
            onClick={() => {
              this.onBlockForm();
              openDialogAction({
                dialog: ChangeSpendingPasswordDialog,
              });
            }}
          />

          {!isIncentivizedTestnet && (
            <WalletRecoveryPhrase
              recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
              recoveryPhraseVerificationStatus={
                recoveryPhraseVerificationStatus
              }
              recoveryPhraseVerificationStatusType={
                recoveryPhraseVerificationStatusType
              }
              creationDate={creationDate}
              openDialogAction={openDialogAction}
              isDialogOpen={isDialogOpen}
              walletRecoveryPhraseStep1Container={
                walletRecoveryPhraseStep1Container
              }
              walletRecoveryPhraseStep2Container={
                walletRecoveryPhraseStep2Container
              }
              walletRecoveryPhraseStep3Container={
                walletRecoveryPhraseStep3Container
              }
              walletRecoveryPhraseStep4Container={
                walletRecoveryPhraseStep4Container
              }
            />
          )}

          <div className={styles.resyncWalletBox}>
            <span>{intl.formatMessage(messages.resyncWalletHeader)}</span>
            <div className={styles.contentBox}>
              <p>{intl.formatMessage(messages.resyncWalletDescription)}</p>
              <ResyncWalletButton
                isSubmitting={isForcedWalletResyncStarting}
                onClick={onResyncWallet}
              />
            </div>
          </div>

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
        </BorderedBox>

        <BorderedBox className={styles.deleteWalletBox}>
          <span>{intl.formatMessage(messages.deleteWalletHeader)}</span>
          <div className={styles.contentBox}>
            <div>
              <p>{intl.formatMessage(messages.deleteWalletWarning1)}</p>
              <p>{intl.formatMessage(messages.deleteWalletWarning2)}</p>
            </div>
            <DeleteWalletButton
              onClick={() => {
                this.onBlockForm();
                openDialogAction({
                  dialog: DeleteWalletConfirmationDialog,
                });
              }}
            />
          </div>
        </BorderedBox>

        {isDialogOpen(ChangeSpendingPasswordDialog)
          ? changeSpendingPasswordDialog
          : false}

        {isDialogOpen(DeleteWalletConfirmationDialog)
          ? deleteWalletDialogContainer
          : false}
      </div>
    );
  }
}
