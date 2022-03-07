import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
// Helpers
import { defineMessages } from 'react-intl';
import StoryDecorator from '../../_support/StoryDecorator';
import type { WalletSettingRemoveMessages } from '../../../../source/renderer/app/components/wallet/settings/WalletSettingsRemoveConfirmationDialog';
// Screens
import WalletSettingsActionConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/WalletSettingsRemoveConfirmationDialog';

const messages: WalletSettingRemoveMessages = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.unpair.dialog.title',
    defaultMessage: '!!!Unpair Wallet',
    description: 'Title for the "Unpair wallet" dialog.',
  },
  confirmButtonLabel: {
    id: 'wallet.settings.unpair.dialog.confirmButtonLabel',
    defaultMessage: '!!!Delete',
    description:
      'Label for the "Unpair (x)" button in the unpair wallet dialog.',
  },
  confirmationQuestion: {
    id: 'wallet.settings.unpair.dialog.confirmationQuestion',
    defaultMessage:
      '!!!Do you really want to unpair <strong>{walletName}</strong> wallet?',
    description: 'Question if the user really wants to unpair the wallet.',
  },
});
storiesOf('Wallets|Settings', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>) // ====== Stories ======
  .add('Unpair - Accepted', () => (
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
        isUnpair
        onConfirmationValueChange={action('onRecoveryWordChange')}
        isSubmitting={false}
      />
    </div>
  ))
  .add('Unpair - Accepted & submitting', () => (
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
        isUnpair
        onConfirmationValueChange={action('onRecoveryWordChange')}
        isSubmitting
      />
    </div>
  ));
