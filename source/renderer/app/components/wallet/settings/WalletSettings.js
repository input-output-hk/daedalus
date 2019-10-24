// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import LocalizableError from '../../../i18n/LocalizableError';
import BorderedBox from '../../widgets/BorderedBox';
import InlineEditingInput from '../../widgets/forms/InlineEditingInput';
import InlineEditingDropdown from '../../widgets/forms/InlineEditingDropdown';
import ReadOnlyInput from '../../widgets/forms/ReadOnlyInput';
import DeleteWalletButton from './DeleteWalletButton';
import DeleteWalletConfirmationDialog from './DeleteWalletConfirmationDialog';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import ChangeSpendingPasswordDialog from './ChangeSpendingPasswordDialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletSettings.scss';
import WalletRecoveryPhrase from './WalletRecoveryPhrase';

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
  deleteWalletWarning: {
    id: 'wallet.settings.deleteWallet.warning',
    defaultMessage:
      '!!!Once you delete a wallet, there is no going back. The only way to restore your wallet is to use your recovery phrase.',
    description: 'Delete wallet warning explaining the consequences.',
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
  assuranceLevels: Array<{ value: string, label: ReactIntlMessage }>,
  walletName: string,
  creationDate: Date,
  walletAssurance: string,
  isSpendingPasswordSet: boolean,
  spendingPasswordUpdateDate: ?Date,
  error?: ?LocalizableError,
  openDialogAction: Function,
  isDialogOpen: Function,
  onFieldValueChange: Function,
  onStartEditing: Function,
  onStopEditing: Function,
  onCancelEditing: Function,
  nameValidator: Function,
  activeField: ?string,
  isSubmitting: boolean,
  isInvalid: boolean,
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
};

@observer
export default class WalletSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentWillUnmount() {
    // This call is used to prevent display of old successfully-updated messages
    this.props.onCancelEditing();
  }

  render() {
    const { intl } = this.context;
    const {
      assuranceLevels,
      walletAssurance,
      walletName,
      creationDate,
      isSpendingPasswordSet,
      spendingPasswordUpdateDate,
      error,
      openDialogAction,
      isDialogOpen,
      onFieldValueChange,
      onStartEditing,
      onStopEditing,
      onCancelEditing,
      nameValidator,
      activeField,
      isSubmitting,
      isInvalid,
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
    } = this.props;

    const assuranceLevelOptions = assuranceLevels.map(assurance => ({
      value: assurance.value,
      label: intl.formatMessage(assurance.label),
    }));

    const passwordMessage = isSpendingPasswordSet
      ? intl.formatMessage(messages.passwordLastUpdated, {
          lastUpdated: moment(spendingPasswordUpdateDate).fromNow(),
        })
      : intl.formatMessage(messages.passwordNotSet);

    return (
      <div className={styles.component}>
        <BorderedBox>
          <InlineEditingInput
            className="walletName"
            inputFieldLabel={intl.formatMessage(messages.name)}
            inputFieldValue={walletName}
            isActive={activeField === 'name'}
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
          />

          <InlineEditingDropdown
            className="walletAssuranceLevel"
            label={intl.formatMessage(messages.assuranceLevelLabel)}
            options={assuranceLevelOptions}
            value={walletAssurance}
            isActive={activeField === 'assurance'}
            onStartEditing={() => onStartEditing('assurance')}
            onStopEditing={onStopEditing}
            onSubmit={value => onFieldValueChange('assurance', value)}
            successfullyUpdated={
              !isSubmitting && lastUpdatedField === 'assurance'
            }
          />

          <ReadOnlyInput
            label={intl.formatMessage(messages.passwordLabel)}
            value={passwordMessage}
            isSet={isSpendingPasswordSet}
            onClick={() =>
              openDialogAction({
                dialog: ChangeSpendingPasswordDialog,
              })
            }
          />

          <WalletRecoveryPhrase
            recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
            recoveryPhraseVerificationStatus={recoveryPhraseVerificationStatus}
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

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
        </BorderedBox>

        <BorderedBox className={styles.deleteWalletBox}>
          <span>{intl.formatMessage(messages.deleteWalletHeader)}</span>
          <div className={styles.contentBox}>
            <p>{intl.formatMessage(messages.deleteWalletWarning)}</p>
            <DeleteWalletButton
              onClick={() =>
                openDialogAction({
                  dialog: DeleteWalletConfirmationDialog,
                })
              }
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
