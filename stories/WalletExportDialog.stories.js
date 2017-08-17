import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import WalletExportDialog from '../app/components/wallet/settings/export-to-file/WalletExportToFileDialog';

storiesOf('WalletExportDialog', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('default', () => (
    <div>
      <WalletExportDialog
        walletName="Test Wallet"
        onClose={action('onClose')}
      />
    </div>
  ))

  .add('spending password', () => (
  <div>
    <WalletExportDialog
      walletName="Test Wallet"
      hasSpendingPassword
      onSubmit={action('onSubmit')}
      onClose={action('onClose')}
    />
  </div>
));
