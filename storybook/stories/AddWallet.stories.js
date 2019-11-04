// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import wordlist from 'bip39/wordlists/english';
import StoryDecorator from './support/StoryDecorator';
import WalletAdd from '../../source/renderer/app/components/wallet/WalletAdd';
import WalletRestoreDialog from '../../source/renderer/app/components/wallet/WalletRestoreDialog';
import WalletCreateDialog from '../../source/renderer/app/components/wallet/WalletCreateDialog';
import WalletFileImportDialog from '../../source/renderer/app/components/wallet/file-import/WalletFileImportDialog';

storiesOf('AddWallet', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('WalletAdd', () => (
    <div>
      <WalletAdd
        onCreate={() => {}}
        onImportFile={() => {}}
        onRestore={() => {}}
        isRestoreActive={false}
        isMaxNumberOfWalletsReached={false}
      />
    </div>
  ))

  .add('WalletCreateDialog - old', () => (
    <div>
      <WalletCreateDialog
        onSubmit={action('onSubmit')}
        onCancel={action('onClose')}
      />
    </div>
  ))

  .add('WalletRestoreDialog', () => (
    <div>
      <WalletRestoreDialog
        mnemonicValidator={() => {}}
        showCertificateRestore={false}
        isSubmitting={false}
        onSubmit={action('onSubmit')}
        onCancel={action('onClose')}
        suggestedMnemonics={wordlist}
        onChoiceChange={() => {}}
      />
    </div>
  ))

  .add('WalletFileImportDialog', () => (
    <div>
      <WalletFileImportDialog
        isSubmitting={false}
        onSubmit={action('onSubmit')}
        onClose={action('onClose')}
        error={null}
      />
    </div>
  ));
