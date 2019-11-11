// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';

// Helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Screens
import DeleteWalletConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/DeleteWalletConfirmationDialog';

storiesOf('Wallets|Actions|Delete', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('without funds & countdown', () => (
    <div>
      <DeleteWalletConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds={false}
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
  .add('without funds - not accepted', () => (
    <div>
      <DeleteWalletConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds={false}
        countdownFn={() => 0}
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
  .add('without funds - accepted', () => (
    <div>
      <DeleteWalletConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds={false}
        countdownFn={() => 0}
        isBackupNoticeAccepted
        confirmationValue="babushka"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={() => {}}
        isSubmitting={false}
      />
    </div>
  ))
  .add('funds & countdown', () => (
    <div>
      <DeleteWalletConfirmationDialog
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
  .add('funds & accepted', () => (
    <div>
      <DeleteWalletConfirmationDialog
        walletName="My Wallet"
        hasWalletFunds
        countdownFn={() => 0}
        isBackupNoticeAccepted
        confirmationValue="babushka"
        onAcceptBackupNotice={() => {}}
        onContinue={() => {}}
        onCancel={() => {}}
        onConfirmationValueChange={() => {}}
        isSubmitting={false}
      />
    </div>
  ))
  .add('funds & accepted & filled', () => (
    <div>
      <DeleteWalletConfirmationDialog
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
  ));
