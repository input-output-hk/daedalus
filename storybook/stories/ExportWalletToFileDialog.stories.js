// @flow
import React from 'react';
import { storiesOf, action } from '@storybook/react';
import StoryDecorator from './support/StoryDecorator';
import ExportWalletToFileDialog from '../../source/renderer/app/components/wallet/settings/ExportWalletToFileDialog';

storiesOf('ExportWalletToFileDialog', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('default', () => (
    <div>
      <ExportWalletToFileDialog
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
      <ExportWalletToFileDialog
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
      <ExportWalletToFileDialog
        walletName="Test Wallet"
        hasSpendingPassword
        isSubmitting={false}
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ));
