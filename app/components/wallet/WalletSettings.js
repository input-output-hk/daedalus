// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import environment from '../../environment';
import LocalizableError from '../../i18n/LocalizableError';
import BorderedBox from '../widgets/BorderedBox';
import InlineEditingInput from '../widgets/forms/InlineEditingInput';
import InlineEditingDropdown from '../widgets/forms/InlineEditingDropdown';
import ReadOnlyInput from '../widgets/forms/ReadOnlyInput';
import DeleteWalletButton from './settings/DeleteWalletButton';
import DeleteWalletConfirmationDialog from './settings/DeleteWalletConfirmationDialog';
import DeleteWalletDialogContainer from '../../containers/wallet/dialogs/DeleteWalletDialogContainer';
import WalletExportDialog from './settings/export-to-file/WalletExportToFileDialog';
import WalletExportToFileDialogContainer from '../../containers/wallet/settings/WalletExportToFileDialogContainer';
/* eslint-disable max-len */
// import ExportPaperWalletPrinterCopyDialog from './settings/paper-wallet-export-dialogs/ExportPaperWalletPrinterCopyDialog';
// import ExportPaperWalletPrinterCopyDialogContainer from '../../containers/wallet/dialogs/paper-wallet-export/ExportPaperWalletPrinterCopyDialogContainer';
// import ExportPaperWalletMnemonicDialog from './settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicDialog';
// import ExportPaperWalletMnemonicDialogContainer from '../../containers/wallet/dialogs/paper-wallet-export/ExportPaperWalletMnemonicDialogContainer';
// import ExportPaperWalletMnemonicVerificationDialog from './settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicVerificationDialog';
// import ExportPaperWalletMnemonicVerificationDialogContainer from '../../containers/wallet/dialogs/paper-wallet-export/ExportPaperWalletMnemonicVerificationDialogContainer';
// import ExportPaperWalletCertificateDialog from './settings/paper-wallet-export-dialogs/ExportPaperWalletCertificateDialog';
// import ExportPaperWalletCertificateDialogContainer from '../../containers/wallet/dialogs/paper-wallet-export/ExportPaperWalletCertificateDialogContainer';
/* eslint-disable max-len */
import type { ReactIntlMessage } from '../../types/i18nTypes';
import ChangeWalletPasswordDialog from './settings/ChangeWalletPasswordDialog';
import ChangeWalletPasswordDialogContainer from '../../containers/wallet/dialogs/ChangeWalletPasswordDialogContainer';
import globalMessages from '../../i18n/global-messages';
import styles from './WalletSettings.scss';

export const messages = defineMessages({
  name: {
    id: 'wallet.settings.name.label',
    defaultMessage: '!!!Name',
    description: 'Label for the "Name" text input on the wallet settings page.',
  },
  assuranceLevelLabel: {
    id: 'wallet.settings.assurance',
    defaultMessage: '!!!Transaction assurance security level',
    description: 'Label for the "Transaction assurance security level" dropdown.',
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
    defaultMessage: '!!!You still don\'t have password',
    description: 'You still don\'t have password set message.',
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
  walletAssurance: string,
  isWalletPasswordSet: boolean,
  walletPasswordUpdateDate: ?Date,
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
      assuranceLevels, walletAssurance,
      walletName, isWalletPasswordSet,
      walletPasswordUpdateDate, error,
      openDialogAction, isDialogOpen,
      onFieldValueChange, onStartEditing,
      onStopEditing, onCancelEditing,
      nameValidator, activeField,
      isSubmitting, isInvalid,
      lastUpdatedField,
    } = this.props;

    const assuranceLevelOptions = assuranceLevels.map(assurance => ({
      value: assurance.value,
      label: intl.formatMessage(assurance.label),
    }));

    const passwordMessage = isWalletPasswordSet ? (
      intl.formatMessage(messages.passwordLastUpdated, {
        lastUpdated: moment(walletPasswordUpdateDate).fromNow(),
      })
    ) : intl.formatMessage(messages.passwordNotSet);

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
            onSubmit={(value) => onFieldValueChange('name', value)}
            isValid={nameValidator}
            validationErrorMessage={intl.formatMessage(globalMessages.invalidWalletName)}
            successfullyUpdated={!isSubmitting && lastUpdatedField === 'name' && !isInvalid}
          />

          <InlineEditingDropdown
            className="walletAssuranceLevel"
            label={intl.formatMessage(messages.assuranceLevelLabel)}
            options={assuranceLevelOptions}
            value={walletAssurance}
            isActive={activeField === 'assurance'}
            onStartEditing={() => onStartEditing('assurance')}
            onStopEditing={onStopEditing}
            onSubmit={(value) => onFieldValueChange('assurance', value)}
            successfullyUpdated={!isSubmitting && lastUpdatedField === 'assurance'}
          />

          <ReadOnlyInput
            label={intl.formatMessage(messages.passwordLabel)}
            value={passwordMessage}
            isSet={isWalletPasswordSet}
            onClick={() => openDialogAction({
              dialog: ChangeWalletPasswordDialog,
            })}
          />

          {/*
            <div className={styles.export}>
              <h2>Export</h2>
              <p>
                Use your wallet on multiple devices
                or give read-only copies to friends.
              </p>
              <button
                className={styles.export_link}
                onClick={() => openDialogAction({
                  dialog: WalletExportDialog
                })}
              >
                {intl.formatMessage(messages.exportButtonLabel)}
              </button>
            </div>
          */}

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <div className={styles.actionButtons}>
            {!environment.isMainnet() ? (
              <button
                className={styles.exportLink}
                onClick={() => openDialogAction({
                  dialog: WalletExportDialog
                })}
              >
                {intl.formatMessage(messages.exportButtonLabel)}
              </button>
            ) : null}

            <DeleteWalletButton
              onClick={() => openDialogAction({
                dialog: DeleteWalletConfirmationDialog,
              })}
            />
          </div>

        </BorderedBox>

        {isDialogOpen(ChangeWalletPasswordDialog) ? (
          <ChangeWalletPasswordDialogContainer />
        ) : null}

        {isDialogOpen(DeleteWalletConfirmationDialog) ? (
          <DeleteWalletDialogContainer />
        ) : null}

        {isDialogOpen(WalletExportDialog) ? (
          <WalletExportToFileDialogContainer />
        ) : null}

        {/*
          {isDialogOpen(ExportPaperWalletPrinterCopyDialog) ? (
            <ExportPaperWalletPrinterCopyDialogContainer />
          ) : null}

          {isDialogOpen(ExportPaperWalletMnemonicDialog) ? (
            <ExportPaperWalletMnemonicDialogContainer />
          ) : null}

          {isDialogOpen(ExportPaperWalletMnemonicVerificationDialog) ? (
            <ExportPaperWalletMnemonicVerificationDialogContainer />
          ) : null}

          {isDialogOpen(ExportPaperWalletCertificateDialog) ? (
            <ExportPaperWalletCertificateDialogContainer />
          ) : null}
        */}

      </div>
    );
  }

}
