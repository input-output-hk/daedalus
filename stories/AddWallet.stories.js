import React from 'react';
import { storiesOf } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import WalletAddDialog from '../app/components/wallet/WalletAddDialog';
import WalletImportDialog from '../app/components/wallet/WalletImportDialog';

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

  .add('WalletImportDialog', () => (
    <div>
      <WalletImportDialog
      />
    </div>
  ));

