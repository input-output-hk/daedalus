import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from './support/StoryDecorator';
import WalletAddDialog from '../../source/renderer/app/components/wallet/WalletAddDialog';
import WalletRestoreDialog from '../../source/renderer/app/components/wallet/WalletRestoreDialog';
import WalletFileImportDialog from '../../source/renderer/app/components/wallet/file-import/WalletFileImportDialog';

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

