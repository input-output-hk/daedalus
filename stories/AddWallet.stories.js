import React from 'react';
import { storiesOf } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import WalletAddDialog from '../app/components/wallet/WalletAddDialog';

storiesOf('AddWallet', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('CheckboxWithLongLabel - checked', () => (
    <div>
      <WalletAddDialog
        onCreate={() => {}}
        onImport={() => {}}
      />
    </div>
  ));

