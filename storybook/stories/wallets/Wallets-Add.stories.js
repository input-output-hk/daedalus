// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';

// Assets and helpers
import WalletsWrapper from './WalletsWrapper';

// Screens
import WalletAdd from '../../../source/renderer/app/components/wallet/WalletAdd';
import WalletRestoreDialog from '../../../source/renderer/app/components/wallet/WalletRestoreDialog';
import WalletFileImportDialog from '../../../source/renderer/app/components/wallet/file-import/WalletFileImportDialog';
import WalletsCreateWallet from './Wallets-Add-Create.stories';

/* eslint-disable consistent-return */
storiesOf('WALLETS|Add wallet', module)
  .addDecorator(WalletsWrapper)

  // ====== Stories ======

  .add('Add', () => (
    <WalletAdd
      onCreate={() => {}}
      onRestore={() => {}}
      onImportFile={() => {}}
      isRestoreActive={boolean('isRestoreActive', false)}
      isMainnet={boolean('isMainnet', false)}
      isTestnet={boolean('isTestnet', false)}
      isMaxNumberOfWalletsReached={boolean(
        'isMaxNumberOfWalletsReached',
        false
      )}
    />
  ))

  .add('Create', () => <WalletsCreateWallet />)

  .add('Restore', () => (
    <WalletRestoreDialog
      onSubmit={action('onSubmit')}
      onCancel={action('onCancel')}
      isSubmitting={boolean('isSubmitting', false)}
      mnemonicValidator={action('mnemonicValidator')}
      suggestedMnemonics={[]}
      onChoiceChange={action('onChoiceChange')}
    />
  ))

  .add('Import', () => (
    <WalletFileImportDialog
      isSubmitting={false}
      onSubmit={action('onSubmit')}
      onClose={action('onClose')}
      error={null}
    />
  ));
