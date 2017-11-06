import React from 'react';
import { storiesOf, action } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import WalletAddDialog from '../app/components/wallet/WalletAddDialog';
import WalletRestoreDialog from '../app/components/wallet/WalletRestoreDialog';
import WalletFileImportDialog from '../app/components/wallet/file-import/WalletFileImportDialog';

storiesOf('AddWallet', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('WalletAddDialog', () => (
    <div>
      <WalletAddDialog
        onCreate={() => {}}
        onImport={() => {}}
      />
    </div>
  ))

  .add('WalletRestoreDialog', () => (
    <div>
      <WalletRestoreDialog
        mnemonicValidator={() => {}}
      />
    </div>
  ))

  .add('WalletFileImportDialog', () => (
    <div>
      <WalletFileImportDialog
        isSubmitting={false}
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
      />
    </div>
  ));

