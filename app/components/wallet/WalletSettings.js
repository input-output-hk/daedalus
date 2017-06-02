// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import Select from 'react-polymorph/lib/components/Select';
import SelectSkin from 'react-polymorph/lib/skins/simple/SelectSkin';
import LocalizableError from '../../i18n/LocalizableError';
import BorderedBox from '../widgets/BorderedBox';
import styles from './WalletSettings.scss';
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

const messages = defineMessages({
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
    walletAssurance: string,
    onWalletAssuranceLevelUpdate: Function,
    isWalletPasswordSet: boolean,
    walletPasswordUpdateDate: ?Date,
    error?: ?LocalizableError,
    openDialogAction: Function,
    isDialogOpen: Function,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      assuranceLevels, walletAssurance,
      onWalletAssuranceLevelUpdate,
      isWalletPasswordSet,
      walletPasswordUpdateDate, error,
      openDialogAction, isDialogOpen,
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
