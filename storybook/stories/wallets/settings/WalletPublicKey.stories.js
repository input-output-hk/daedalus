// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';

// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

// Components
import WalletPublicKeyDialog from '../../../../source/renderer/app/components/wallet/settings/WalletPublicKeyDialog';

storiesOf('Wallets|Settings', module)
  .addDecorator(WalletsWrapper)
  .add('Public Key - Spending Password', () => (
    <WalletPublicKeyDialog
      onRevealPublicKey={action('onRevealPublicKey')}
      onCancel={action('onCancel')}
      error={null}
    />
  ));
