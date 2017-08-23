import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import WalletExportToFileDialog from '../app/components/wallet/settings/export-to-file/WalletExportToFileDialog';

storiesOf('WalletExportToFileDialog', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('default', () => (
    <div>
      <WalletExportToFileDialog
        walletName="Test Wallet"
        onClose={action('onClose')}
        isSubmitting={false}
      />
    </div>
  ))

  .add('submitting', () => (
    <div>
      <WalletExportToFileDialog walletName="Test Wallet" isSubmitting />
    </div>
  ))

  .add('spending password', () => (
    <div>
      <WalletExportToFileDialog
        walletName="Test Wallet"
        hasSpendingPassword
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
        isSubmitting={false}
      />
    </div>
  ));
