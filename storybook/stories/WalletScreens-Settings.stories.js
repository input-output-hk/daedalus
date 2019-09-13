// @flow
import React from 'react';
import { text, boolean, number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import moment from 'moment';

// Screens
import WalletSettings from '../../source/renderer/app/components/wallet/settings/WalletSettings';
import ChangeSpendingPasswordDialog from '../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import DeleteWalletConfirmationDialog from '../../source/renderer/app/components/wallet/settings/DeleteWalletConfirmationDialog';
import ExportWalletToFileDialog from '../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';

/* eslint-disable react/display-name  */

export default () => (
  <WalletSettings
    activeField={null}
    isDialogOpen={dialog => {
      if (dialog === ChangeSpendingPasswordDialog) {
        return boolean('Change Password - Show dialog', false);
      }
      if (dialog === DeleteWalletConfirmationDialog) {
        return boolean('Delete Wallet - Show dialog', false);
      }
      if (dialog === ExportWalletToFileDialog) {
        return boolean('Export Wallet - Show dialog', false);
      }
      return false;
    }}
    isInvalid={false}
    isSubmitting={false}
    isSpendingPasswordSet={boolean('isSpendingPasswordSet', false)}
    lastUpdatedField={null}
    nameValidator={() => true}
    onCancelEditing={() => {}}
    onFieldValueChange={() => {}}
    onStartEditing={() => {}}
    onStopEditing={() => {}}
    openDialogAction={() => {}}
    walletName={text('Wallet Name', 'Wallet Name')}
    spendingPasswordUpdateDate={moment()
      .subtract(1, 'month')
      .toDate()}
    changeSpendingPasswordDialog={
      <ChangeSpendingPasswordDialog
        currentPasswordValue="current"
        newPasswordValue="new"
        repeatedPasswordValue="new"
        isSpendingPasswordSet={boolean('isSpendingPasswordSet', false)}
        onSave={action('Change Password - onSave')}
        onCancel={action('Change Password - onCancel')}
        onPasswordSwitchToggle={action(
          'Change Password - onPasswordSwitchToggle'
        )}
        onDataChange={action('Change Password - onDataChange')}
        isSubmitting={boolean('Change Password - isSubmitting', false)}
        error={null}
      />
    }
    deleteWalletDialogContainer={
      <DeleteWalletConfirmationDialog
        walletName={text(
          'DeleteWalletConfirmationDialog: Wallet Name',
          'Wallet To Delete'
        )}
        hasWalletFunds={boolean('hasWalletFunds', false)}
        countdownFn={() => number('Delete Wallet Countdown', 9)}
        isBackupNoticeAccepted={boolean('isBackupNoticeAccepted', false)}
        onAcceptBackupNotice={action('Delete Wallet - onAcceptBackupNotice')}
        onContinue={action('Delete Wallet - onContinue')}
        onCancel={action('Delete Wallet - onCancel')}
        confirmationValue={text('Delete Wallet Confirmation Value')}
        onConfirmationValueChange={action(
          'Delete Wallet - onConfirmationValueChange'
        )}
        isSubmitting={boolean('Delete Wallet - isSubmitting', false)}
      />
    }
    exportWalletDialogContainer={
      <ExportWalletToFileDialog
        walletName={text('Wallet Name', 'Wallet Name')}
        hasSpendingPassword={boolean('isSpendingPasswordSet', false)}
        isSubmitting={boolean('Export Wallet - isSubmitting', false)}
        onSubmit={action('Export Wallet - onSubmit')}
        onClose={action('Export Wallet - onClose')}
      />
    }
  />
);
