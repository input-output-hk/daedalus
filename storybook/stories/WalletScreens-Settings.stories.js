// @flow
import React from 'react';
import { text, boolean, number, select } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import moment from 'moment';

// Screens
import WalletSettings from '../../source/renderer/app/components/wallet/settings/WalletSettings';
import { WalletAssuranceModeOptions } from '../../source/renderer/app/domains/Wallet';
import ChangeSpendingPasswordDialog from '../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import DeleteWalletConfirmationDialog from '../../source/renderer/app/components/wallet/settings/DeleteWalletConfirmationDialog';
import ExportWalletToFileDialog from '../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';
import WalletRecoveryPhraseStep1Dialog from '../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from '../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import {
  MNEMONICS_CHECKING_WARNING,
  MNEMONICS_CHECKING_NOTIFICATION,
} from '../../source/renderer/app/config/walletsConfig';

/* eslint-disable react/display-name  */

const mnemonicsConfirmationDateOptions = {
  Ok: new Date(),
  Warning: moment()
    .subtract(MNEMONICS_CHECKING_WARNING + 10, 'days')
    .toDate(),
  Notification: moment()
    .subtract(MNEMONICS_CHECKING_NOTIFICATION + 10, 'days')
    .toDate(),
};

const basicSettingsId = 'Basic Settings';
const changePasswordId = 'Change Password';
const deleteWalletId = 'Delete Wallet';
const exportWalletId = 'Export Wallet';
const recoveryPhraseId = 'Recovery Phrase';

export default () => (
  <WalletSettings
    isDialogOpen={dialog => {
      if (dialog === ChangeSpendingPasswordDialog) {
        return boolean(
          'Change Password - Show dialog',
          false,
          changePasswordId
        );
      }
      if (dialog === DeleteWalletConfirmationDialog) {
        return boolean('Delete Wallet - Show dialog', false, deleteWalletId);
      }
      if (dialog === ExportWalletToFileDialog) {
        return boolean('Export Wallet - Show dialog', false, exportWalletId);
      }
      if (dialog === WalletRecoveryPhraseStep1Dialog) {
        return boolean(
          'Recovery Phrase - Step 1 Dialog',
          false,
          recoveryPhraseId
        );
      }
      // if (dialog === WalletRecoveryPhraseStep2Dialog) {
      //   return boolean('Recovery Phrase - Step 2 Dialog', false, recoveryPhraseId);
      // }
      return false;
    }}
    activeField={null}
    assuranceLevels={[
      {
        value: WalletAssuranceModeOptions.NORMAL,
        label: {
          id: 'global.assuranceLevel.normal',
          defaultMessage: '!!!Normal',
          description: '',
        },
      },
      {
        value: WalletAssuranceModeOptions.STRICT,
        label: {
          id: 'global.assuranceLevel.strict',
          defaultMessage: '!!!Strict',
          description: '',
        },
      },
    ]}
    isInvalid={false}
    isSubmitting={false}
    lastUpdatedField={null}
    nameValidator={() => true}
    onCancelEditing={() => {}}
    onFieldValueChange={() => {}}
    onStartEditing={() => {}}
    onStopEditing={() => {}}
    openDialogAction={() => {}}
    walletAssurance={WalletAssuranceModeOptions.NORMAL}
    walletName={text('Wallet Name', 'Wallet Name', basicSettingsId)}
    spendingPasswordUpdateDate={moment()
      .subtract(1, 'month')
      .toDate()}
    isSpendingPasswordSet={boolean(
      'isSpendingPasswordSet',
      false,
      changePasswordId
    )}
    changeSpendingPasswordDialog={
      <ChangeSpendingPasswordDialog
        currentPasswordValue="current"
        newPasswordValue="new"
        repeatedPasswordValue="new"
        isSpendingPasswordSet={boolean(
          'isSpendingPasswordSet',
          false,
          changePasswordId
        )}
        onSave={action('Change Password - onSave')}
        onCancel={action('Change Password - onCancel')}
        onPasswordSwitchToggle={action(
          'Change Password - onPasswordSwitchToggle'
        )}
        onDataChange={action('Change Password - onDataChange')}
        isSubmitting={boolean(
          'Change Password - isSubmitting',
          false,
          changePasswordId
        )}
        error={null}
      />
    }
    deleteWalletDialogContainer={
      <DeleteWalletConfirmationDialog
        walletName={text(
          'DeleteWalletConfirmationDialog: Wallet Name',
          'Wallet To Delete',
          deleteWalletId
        )}
        hasWalletFunds={boolean('hasWalletFunds', false, basicSettingsId)}
        countdownFn={() => number('Delete Wallet Countdown', 9, deleteWalletId)}
        isBackupNoticeAccepted={boolean(
          'isBackupNoticeAccepted',
          false,
          basicSettingsId
        )}
        onAcceptBackupNotice={action('Delete Wallet - onAcceptBackupNotice')}
        onContinue={action('Delete Wallet - onContinue')}
        onCancel={action('Delete Wallet - onCancel')}
        confirmationValue={text(
          'Delete Wallet Confirmation Value',
          'Wallet name',
          deleteWalletId
        )}
        onConfirmationValueChange={action(
          'Delete Wallet - onConfirmationValueChange'
        )}
        isSubmitting={boolean(
          'Delete Wallet - isSubmitting',
          false,
          deleteWalletId
        )}
      />
    }
    exportWalletDialogContainer={
      <ExportWalletToFileDialog
        walletName={text('Wallet Name', 'Wallet Name')}
        hasSpendingPassword={boolean(
          'isSpendingPasswordSet',
          false,
          basicSettingsId
        )}
        isSubmitting={boolean(
          'Export Wallet - isSubmitting',
          false,
          exportWalletId
        )}
        onSubmit={action('Export Wallet - onSubmit')}
        onClose={action('Export Wallet - onClose')}
      />
    }
    walletRecoveryPhraseStep1Container={<WalletRecoveryPhraseStep1Dialog />}
    walletRecoveryPhraseStep2Container={<WalletRecoveryPhraseStep2Dialog />}
    mnemonicsConfirmationDate={select(
      'mnemonicsConfirmationDate',
      mnemonicsConfirmationDateOptions,
      'Ok',
      recoveryPhraseId
    )}
  />
);
