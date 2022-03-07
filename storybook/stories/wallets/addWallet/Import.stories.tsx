import React from 'react';
import { action } from '@storybook/addon-actions';
import { storiesOf } from '@storybook/react';
// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
import WalletFileImportDialog from '../../../../source/renderer/app/components/wallet/file-import/WalletFileImportDialog';

storiesOf('Wallets|Add Wallet', module)
  .addDecorator(WalletsWrapper)
  .add('Import', () => (
    <WalletFileImportDialog
      isSubmitting={false}
      onSubmit={action('onSubmit')}
      onClose={action('onClose')}
      error={null}
    />
  ));
