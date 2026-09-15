import React from 'react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
// Screens
import WalletRestoreDialog from '../../../../source/renderer/app/components/wallet/WalletRestoreDialog';

export default {
  title: 'Wallets / Add Wallet',
  decorators: [WalletsWrapper],
};

export const RestoreOld = {
  render: () => (
    <WalletRestoreDialog
      onSubmit={action('onSubmit')}
      onCancel={action('onCancel')}
      isSubmitting={boolean('isSubmitting', false)}
      mnemonicValidator={action('mnemonicValidator')}
      suggestedMnemonics={[]}
      onChoiceChange={action('onChoiceChange')}
    />
  ),

  name: 'Restore - Old',
};
