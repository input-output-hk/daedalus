import React from 'react';
import { boolean, number, text } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import moment from 'moment';
// Screens
import { defineMessages } from 'react-intl';
import ChangeSpendingPasswordDialog from '../../../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import type { WalletSettingRemoveMessages } from '../../../../source/renderer/app/components/wallet/settings/WalletSettingsRemoveConfirmationDialog';
import WalletSettingsActionConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/WalletSettingsRemoveConfirmationDialog';
import WalletRecoveryPhraseStep1Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from '../../../../source/renderer/app/components/wallet/settings/WalletRecoveryPhraseStep4Dialog';

const messages: WalletSettingRemoveMessages = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.delete.dialog.title',
    defaultMessage: '!!!Delete Wallet',
    description: 'Title for the "Delete wallet" dialog.',
  },
  confirmButtonLabel: {
    id: 'wallet.settings.delete.dialog.confirmButtonLabel',
    defaultMessage: '!!!Delete',
    description:
      'Label for the "Delete (x)" button in the delete wallet dialog.',
  },
  confirmationQuestion: {
    id: 'wallet.settings.delete.dialog.confirmationQuestion',
    defaultMessage:
      '!!!Do you really want to delete <strong>{walletName}</strong> wallet?',
    description: 'Question if the user really wants to delete the wallet.',
  },
  confirmBackupNotice: {
    id: 'wallet.settings.delete.dialog.confirmBackupNotice',
    defaultMessage:
      '!!!Make sure you have access to backup before continuing. Otherwise, you will lose all your funds connected to this wallet.',
    description:
      'Notice to confirm if the user has made a backup of his wallet',
  },
  enterRecoveryWordLabel: {
    id: 'wallet.settings.delete.dialog.enterRecoveryWordLabel',
    defaultMessage: '!!!Enter the name of the wallet to confirm deletion:',
    description: 'Instruction for recovery word on delete wallet dialog',
  },
});
export const defaultProps = {
  isDialogOpen: () => {},
  creationDate: new Date(),
  recoveryPhraseVerificationDate: new Date(),
  recoveryPhraseVerificationStatus: 'ok',
  recoveryPhraseVerificationStatusType: 'alreadyVerified',
  walletRecoveryPhraseStep1Container: (
    <WalletRecoveryPhraseStep1Dialog
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      wordCount={number('wordCount', 15)}
      walletName="My Wallet"
    />
  ),
  walletRecoveryPhraseStep2Container: (
    <WalletRecoveryPhraseStep2Dialog
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      wordCount={number('wordCount', 15)}
      expectedWordCount={15}
      walletName="My Wallet"
    />
  ),
  walletRecoveryPhraseStep3Container: (
    <WalletRecoveryPhraseStep3Dialog
      onClose={action('onClose')}
      walletName="My Wallet"
    />
  ),
  walletRecoveryPhraseStep4Container: (
    <WalletRecoveryPhraseStep4Dialog
      onClose={action('onClose')}
      onContinue={action('onContinue')}
      openExternalLink={action('openExternalLink')}
      walletName="My Wallet"
    />
  ),
  activeField: null,
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
  walletName: text('Wallet Name', 'Wallet Name'),
  spendingPasswordUpdateDate: moment().subtract(1, 'month').toDate(),
  changeSpendingPasswordDialog: (
    <ChangeSpendingPasswordDialog
      walletName={text('Wallet Name', 'Wallet Name')}
      currentPasswordValue="current"
      newPasswordValue="new"
      repeatedPasswordValue="new"
      isSpendingPasswordSet={boolean('isSpendingPasswordSet', false)}
      onSave={action('Change Password - onSave')}
      onCancel={action('Change Password - onCancel')}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      onPasswordSwitchToggle={action(
        'Change Password - onPasswordSwitchToggle'
      )}
      onDataChange={action('Change Password - onDataChange')}
      isSubmitting={boolean('Change Password - isSubmitting', false)}
      error={null}
      currentLocale={'en-US'}
    />
  ),
  deleteWalletDialogContainer: (
    <WalletSettingsActionConfirmationDialog
      walletName={text(
        'WalletSettingsRemoveConfirmationDialog: Wallet Name',
        'Wallet To Delete'
      )}
      hasWalletFunds={boolean('hasWalletFunds', false)}
      countdownFn={() => number('Delete Wallet Countdown', 9)}
      isBackupNoticeAccepted={boolean('isBackupNoticeAccepted', false)}
      messages={messages}
      onAcceptBackupNotice={action('Delete Wallet - onAcceptBackupNotice')}
      onContinue={action('Delete Wallet - onContinue')}
      onCancel={action('Delete Wallet - onCancel')}
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
      confirmationValue={text('Delete Wallet Confirmation Value')}
      onConfirmationValueChange={action(
        'Delete Wallet - onConfirmationValueChange'
      )}
      isSubmitting={boolean('Delete Wallet - isSubmitting', false)}
    />
  ),
};
