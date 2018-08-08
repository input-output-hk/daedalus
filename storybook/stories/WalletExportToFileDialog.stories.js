// @flow
import React from 'react';
import { storiesOf, action } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import WalletExportToFileDialog from '../../source/renderer/app/components/wallet/settings/export-to-file/WalletExportToFileDialog';

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
        hasSpendingPassword={false}
        isSubmitting={false}
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ))

  .add('submitting', () => (
    <div>
      <WalletExportToFileDialog
        walletName="Test Wallet"
        hasSpendingPassword={false}
        isSubmitting
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ))

  .add('spending password', () => (
    <div>
      <WalletExportToFileDialog
        walletName="Test Wallet"
        hasSpendingPassword
        isSubmitting={false}
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ));
