import React from 'react';
import { action } from '@storybook/addon-actions';
// Helpers
import { defineMessages } from 'react-intl';
import StoryDecorator from '../../_support/StoryDecorator';
import type { WalletSettingRemoveMessages } from '../../../../source/renderer/app/components/wallet/settings/WalletSettingsRemoveConfirmationDialog';
// Screens
import WalletSettingsActionConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/WalletSettingsRemoveConfirmationDialog';

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

export default {
  title: 'Wallets / Settings',
  decorators: [(story) => <StoryDecorator>{story()}</StoryDecorator>],
};

export const DeleteCountdown = {
  render: () => (
    <div>
      <WalletSettingsActionConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds
        countdownFn={() => 10}
        isBackupNoticeAccepted={false}
        messages={messages}
        confirmationValue="babushka"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={() => {}}
        isSubmitting={false}
      />
    </div>
  ),

  name: 'Delete - Countdown',
};

export const DeleteAcceptedFilledIncorrectly = {
  render: () => (
    <div>
      <WalletSettingsActionConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds
        countdownFn={() => 0}
        isBackupNoticeAccepted
        messages={messages}
        confirmationValue="babushka"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={action('onRecoveryWordChange')}
        isSubmitting={false}
      />
    </div>
  ),

  name: 'Delete - Accepted & filled incorrectly',
};

export const DeleteAcceptedFilledCorrectly = {
  render: () => (
    <div>
      <WalletSettingsActionConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds
        countdownFn={() => 0}
        isBackupNoticeAccepted
        messages={messages}
        confirmationValue="My Wallet"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={action('onRecoveryWordChange')}
        isSubmitting={false}
      />
    </div>
  ),

  name: 'Delete - Accepted & filled correctly',
};

export const DeleteAcceptedFilledCorrectlySubmitting = {
  render: () => (
    <div>
      <WalletSettingsActionConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds
        countdownFn={() => 0}
        isBackupNoticeAccepted
        messages={messages}
        confirmationValue="My Wallet"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={action('onRecoveryWordChange')}
        isSubmitting
      />
    </div>
  ),

  name: 'Delete - Accepted, filled correctly & submitting',
};
