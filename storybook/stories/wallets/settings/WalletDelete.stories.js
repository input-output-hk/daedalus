// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';

// Helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Screens
import WalletSettingsActionConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/WalletSettingsActionConfirmationDialog';

storiesOf('Wallets|Settings', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Delete - Countdown', () => (
    <div>
      <WalletSettingsActionConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds
        countdownFn={() => 10}
        isBackupNoticeAccepted={false}
        confirmationValue="babushka"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={() => {}}
        isSubmitting={false}
      />
    </div>
  ))
  .add('Delete - Accepted & filled incorrectly', () => (
    <div>
      <WalletSettingsActionConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds
        countdownFn={() => 0}
        isBackupNoticeAccepted
        confirmationValue="babushka"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={action('onRecoveryWordChange')}
        isSubmitting={false}
      />
    </div>
  ))
  .add('Delete - Accepted & filled correctly', () => (
    <div>
      <WalletSettingsActionConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds
        countdownFn={() => 0}
        isBackupNoticeAccepted
        confirmationValue="My Wallet"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={action('onRecoveryWordChange')}
        isSubmitting={false}
      />
    </div>
  ))
  .add('Delete - Accepted, filled correctly & submitting', () => (
    <div>
      <WalletSettingsActionConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds
        countdownFn={() => 0}
        isBackupNoticeAccepted
        confirmationValue="My Wallet"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={action('onRecoveryWordChange')}
        isSubmitting
      />
    </div>
  ));
