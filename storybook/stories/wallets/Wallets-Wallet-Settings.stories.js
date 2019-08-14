// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { text, boolean, number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import moment from 'moment';

// Support
import WalletWrapper from '../_support/WalletWrapper';

// Screens
import WalletSettings from '../../../source/renderer/app/components/wallet/settings/WalletSettings';
import { WalletAssuranceModeOptions } from '../../../source/renderer/app/domains/Wallet';
import ChangeSpendingPasswordDialog from '../../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import DeleteWalletConfirmationDialog from '../../../source/renderer/app/components/wallet/settings/DeleteWalletConfirmationDialog';
import ExportWalletToFileDialog from '../../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';

const defaultProps = {
  isDialogOpen: () => {},
  activeField: null,
  assuranceLevels: [
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
  ],
  isInvalid: boolean('isInvalid', false),
  isSubmitting: boolean('isSubmitting', false),
  isSpendingPasswordSet: boolean('isSpendingPasswordSet', false),
  lastUpdatedField: null,
  nameValidator: () => true,
  onCancelEditing: () => {},
  onFieldValueChange: () => {},
  onStartEditing: () => {},
  onStopEditing: () => {},
  openDialogAction: () => {},
  walletAssurance: WalletAssuranceModeOptions.NORMAL,
  walletName: text('Wallet Name', 'Wallet Name'),
  spendingPasswordUpdateDate: moment()
    .subtract(1, 'month')
    .toDate(),
  changeSpendingPasswordDialog: (
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
  ),
  deleteWalletDialogContainer: (
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
  ),
  exportWalletDialogContainer: (
    <ExportWalletToFileDialog
      walletName={text('Wallet Name', 'Wallet Name')}
      hasSpendingPassword={boolean('isSpendingPasswordSet', false)}
      isSubmitting={boolean('Export Wallet - isSubmitting', false)}
      onSubmit={action('Export Wallet - onSubmit')}
      onClose={action('Export Wallet - onClose')}
    />
  ),
};

/* eslint-disable consistent-return */
storiesOf('WALLETS|Wallet/Settings', module)
  .addDecorator(WalletWrapper)

  // ====== Stories ======

  .add('Default', () => <WalletSettings {...defaultProps} />)

  .add('changePassword', () => (
    <WalletSettings
      {...defaultProps}
      isDialogOpen={dialog => {
        if (dialog === ChangeSpendingPasswordDialog) {
          return true;
        }
        return false;
      }}
    />
  ))

  .add('deleteWallet', () => (
    <WalletSettings
      {...defaultProps}
      isDialogOpen={dialog => {
        if (dialog === DeleteWalletConfirmationDialog) {
          return true;
        }
        return false;
      }}
    />
  ))

  .add('exportToFile', () => (
    <WalletSettings
      {...defaultProps}
      isDialogOpen={dialog => {
        if (dialog === ExportWalletToFileDialog) {
          return true;
        }
        return false;
      }}
    />
  ));
