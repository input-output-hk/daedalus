// @flow
import React from 'react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';

// Helpers
import WalletsWrapper from '../utils/WalletsWrapper';

// Screens
import WalletRestoreDialog from '../../../../source/renderer/app/components/wallet/WalletRestoreDialog';

storiesOf('Wallets|Actions', module)
  .addDecorator(WalletsWrapper)
  .add('Restore', () => (
    <WalletRestoreDialog
      onSubmit={action('onSubmit')}
      onCancel={action('onCancel')}
      isSubmitting={boolean('isSubmitting', false)}
      mnemonicValidator={action('mnemonicValidator')}
      suggestedMnemonics={[]}
      onChoiceChange={action('onChoiceChange')}
    />
  ));
