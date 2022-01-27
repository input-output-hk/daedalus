import React from 'react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
// Screens
import WalletRestoreDialog from '../../../../source/renderer/app/components/wallet/WalletRestoreDialog';

storiesOf('Wallets|Add Wallet', module)
  .addDecorator(WalletsWrapper)
  .add('Restore - Old', () => (
    <WalletRestoreDialog
      onSubmit={action('onSubmit')}
      onCancel={action('onCancel')}
      isSubmitting={boolean('isSubmitting', false)}
      mnemonicValidator={action('mnemonicValidator')}
      suggestedMnemonics={[]}
      onChoiceChange={action('onChoiceChange')}
    />
  ));
