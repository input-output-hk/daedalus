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
import WalletRecoveryPhraseWidget from './WalletRecoveryPhraseWidget';
import ResyncWallet from './ResyncWallet';

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
  passwordNotSet: {
    id: 'wallet.settings.passwordNotSet',
    defaultMessage: "!!!You still don't have password",
    description: "You still don't have password set message.",
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
  onRecoveryPhraseVerify: Function,
  nameValidator: Function,
  activeField: ?string,
  isSubmitting: boolean,
  isForcedWalletResyncStarting: boolean,
  isIncentivizedTestnet: boolean,
  isWalletRecoveryPhraseWidgetDisabled?: boolean,
  isInvalid: boolean,
  isLegacy: boolean,
  lastUpdatedField: ?string,
  changeSpendingPasswordDialog: Node,
  deleteWalletDialogContainer: Node,
  recoveryPhraseVerificationDate: ?Date,
  recoveryPhraseVerificationStatus: string,
  recoveryPhraseVerificationStatusType: string,
  locale: string,
  isSpendingPasswordSet: boolean,
  wordCount: number,
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
      onRecoveryPhraseVerify,
      nameValidator,
      activeField,
      isSubmitting,
      isForcedWalletResyncStarting,
      isIncentivizedTestnet,
      isWalletRecoveryPhraseWidgetDisabled,
      isInvalid,
      isLegacy,
      lastUpdatedField,
      changeSpendingPasswordDialog,
      deleteWalletDialogContainer,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
      locale,
      isSpendingPasswordSet,
      wordCount,
    } = this.props;
    const { isFormBlocked } = this.state;

    // Set Japanese locale to moment. Default is en-US
    if (locale === 'ja-JP') {
      moment.locale('ja');
    } else {
      moment.locale('en-us');
    }

    if (isLegacy && isIncentivizedTestnet) {
      const deleteWalletBoxStyles = classNames([
        styles.deleteWalletBox,
        styles.legacyWallet,
      ]);
      return (
        <div className={styles.component}>
          <BorderedBox>
            <ResyncWallet
              isForcedWalletResyncStarting={isForcedWalletResyncStarting}
              onResyncWallet={onResyncWallet}
            />
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

    const passwordMessage = isSpendingPasswordSet
      ? intl.formatMessage(messages.passwordLastUpdated, {
          lastUpdated: moment(spendingPasswordUpdateDate)
            .locale(this.context.intl.locale)
            .fromNow(),
        })
      : intl.formatMessage(messages.passwordNotSet);

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
              !isSubmitting && !isInvalid && lastUpdatedField === 'name'
            }
            inputBlocked={isFormBlocked}
          />

          <ReadOnlyInput
            label={intl.formatMessage(messages.passwordLabel)}
            value={passwordMessage}
            isSet={isSpendingPasswordSet}
            onClick={() => {
              this.onBlockForm();
              openDialogAction({
                dialog: ChangeSpendingPasswordDialog,
              });
            }}
          />

          {!isIncentivizedTestnet && !isWalletRecoveryPhraseWidgetDisabled && (
            <WalletRecoveryPhraseWidget
              onVerify={onRecoveryPhraseVerify}
              recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
              recoveryPhraseVerificationStatus={
                recoveryPhraseVerificationStatus
              }
              recoveryPhraseVerificationStatusType={
                recoveryPhraseVerificationStatusType
              }
              creationDate={creationDate}
              wordCount={wordCount}
            />
          )}

          {isIncentivizedTestnet && (
            <div className={styles.resyncWalletBox}>
              <ResyncWallet
                isForcedWalletResyncStarting={isForcedWalletResyncStarting}
                onResyncWallet={onResyncWallet}
              />
            </div>
          )}

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
