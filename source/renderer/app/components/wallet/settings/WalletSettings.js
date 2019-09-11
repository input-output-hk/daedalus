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
import ExportWalletToFileDialog from './ExportWalletToFileDialog';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import ChangeSpendingPasswordDialog from './ChangeSpendingPasswordDialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletSettings.scss';
import WalletRecoveryPhrase from './WalletRecoveryPhrase';

export const messages = defineMessages({
  name: {
    id: 'wallet.settings.name.label',
    defaultMessage: '!!!Name',
    description: 'Label for the "Name" text input on the wallet settings page.',
  },
  assuranceLevelLabel: {
    id: 'wallet.settings.assurance',
    defaultMessage: '!!!Transaction assurance security level',
    description:
      'Label for the "Transaction assurance security level" dropdown.',
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
  exportButtonLabel: {
    id: 'wallet.settings.exportWalletButtonLabel',
    defaultMessage: '!!!Export wallet',
    description: 'Label for the export button on wallet settings.',
  },
});

type Props = {
  assuranceLevels: Array<{ value: string, label: ReactIntlMessage }>,
  walletName: string,
  walletCreationDate: Date,
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
  showExportLink: boolean,
  lastUpdatedField: ?string,
  changeSpendingPasswordDialog: Node,
  deleteWalletDialogContainer: Node,
  exportWalletDialogContainer: Node,
  walletRecoveryPhraseStep1Container: Node,
  walletRecoveryPhraseStep2Container: Node,
  walletRecoveryPhraseStep3Container: Node,
  walletRecoveryPhraseStep4Container: Node,
  mnemonicsConfirmationDate?: Date,
};

@observer
export default class WalletSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    showExportLink: false,
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
      walletCreationDate,
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
      showExportLink,
      changeSpendingPasswordDialog,
      deleteWalletDialogContainer,
      exportWalletDialogContainer,
      walletRecoveryPhraseStep1Container,
      walletRecoveryPhraseStep2Container,
      walletRecoveryPhraseStep3Container,
      walletRecoveryPhraseStep4Container,
      mnemonicsConfirmationDate,
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
            mnemonicsConfirmationDate={mnemonicsConfirmationDate}
            walletCreationDate={walletCreationDate}
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

          <div className={styles.actionButtons}>
            {showExportLink ? (
              <button
                className={styles.exportLink}
                onClick={() =>
                  openDialogAction({
                    dialog: ExportWalletToFileDialog,
                  })
                }
              >
                {intl.formatMessage(messages.exportButtonLabel)}
              </button>
            ) : (
              false
            )}

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

        {isDialogOpen(ExportWalletToFileDialog)
          ? exportWalletDialogContainer
          : false}
      </div>
    );
  }
}
