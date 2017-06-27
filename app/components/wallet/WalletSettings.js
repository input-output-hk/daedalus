// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import Select from 'react-polymorph/lib/components/Select';
import SelectSkin from 'react-polymorph/lib/skins/simple/SelectSkin';
import LocalizableError from '../../i18n/LocalizableError';
import BorderedBox from '../widgets/BorderedBox';
import InlineEditingInput from '../widgets/forms/InlineEditingInput';
import ReadOnlyInput from '../widgets/forms/ReadOnlyInput';
import DeleteWalletButton from './settings/DeleteWalletButton';
import DeleteWalletConfirmationDialog from './settings/DeleteWalletConfirmationDialog';
import DeleteWalletDialogContainer from '../../containers/wallet/dialogs/DeleteWalletDialogContainer';
import WalletExportDialog from './settings/WalletExportDialog';
import WalletExportDialogContainer from '../../containers/wallet/dialogs/WalletExportDialogContainer';
import ExportPaperWalletPrinterCopyDialog from './settings/paper-wallet-export-dialogs/ExportPaperWalletPrinterCopyDialog';
import ExportPaperWalletPrinterCopyDialogContainer from '../../containers/wallet/dialogs/paper-wallet-export/ExportPaperWalletPrinterCopyDialogContainer';
import ExportPaperWalletMnemonicDialog from './settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicDialog';
import ExportPaperWalletMnemonicDialogContainer from '../../containers/wallet/dialogs/paper-wallet-export/ExportPaperWalletMnemonicDialogContainer';
import ExportPaperWalletMnemonicVerificationDialog from './settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicVerificationDialog';
import ExportPaperWalletMnemonicVerificationDialogContainer from '../../containers/wallet/dialogs/paper-wallet-export/ExportPaperWalletMnemonicVerificationDialogContainer';
import ExportPaperWalletCertificateDialog from './settings/paper-wallet-export-dialogs/ExportPaperWalletCertificateDialog';
import ExportPaperWalletCertificateDialogContainer from '../../containers/wallet/dialogs/paper-wallet-export/ExportPaperWalletCertificateDialogContainer';
import type { ReactIntlMessage } from '../../types/i18nTypes';
import ChangeWalletPasswordDialog from './settings/ChangeWalletPasswordDialog';
import ChangeWalletPasswordDialogContainer from '../../containers/wallet/dialogs/ChangeWalletPasswordDialogContainer';
import globalMessages from '../../i18n/global-messages';
import styles from './WalletSettings.scss';

const messages = defineMessages({
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
});

@observer
export default class WalletSettings extends Component {

  props: {
    assuranceLevels: Array<{ value: string, label: ReactIntlMessage }>,
    walletName: string,
    walletAssurance: string,
    onWalletAssuranceLevelUpdate: Function,
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

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      assuranceLevels, walletAssurance,
      onWalletAssuranceLevelUpdate,
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

          <Select
            className={styles.assuranceLevelSelect}
            label={intl.formatMessage(messages.assuranceLevelLabel)}
            options={assuranceLevelOptions}
            value={walletAssurance}
            onChange={(value) => onWalletAssuranceLevelUpdate({ assurance: value })}
            skin={<SelectSkin />}
          />

          <ReadOnlyInput
            label={intl.formatMessage(messages.passwordLabel)}
            value={passwordMessage}
            isSet={isWalletPasswordSet}
            onClick={() => openDialogAction({
              dialog: ChangeWalletPasswordDialog,
            })}
          />

          {/* TODO: Reactivate for paper wallet export feature!
             <div className={styles.export}>
             <strong>Export</strong>
             <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>
             <p>Maecenas non fringilla velit. Vestibulum ante ipsum primis in
             faucibus orci luctus et ultrices posuere cubilia Curae.</p>
             <button
             className={styles.export_link}
             onClick={() => openDialogAction({
             dialog: WalletExportDialog,
             })}
             >
             export
             </button>
             </div>
           */}

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <div className={styles.deleteWalletButton}>
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
          <WalletExportDialogContainer />
        ) : null}

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

      </div>
    );
  }

}
