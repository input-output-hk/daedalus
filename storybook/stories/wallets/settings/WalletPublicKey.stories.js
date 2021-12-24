// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean } from '@storybook/addon-knobs';

// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

// Components
import WalletPublicKeyDialog from '../../../../source/renderer/app/components/wallet/settings/WalletPublicKeyDialog';

const decorators = [withKnobs, WalletsWrapper];

storiesOf('Wallets/Settings', module).add(
  'Public Key - Spending Password',
  () => (
    <WalletPublicKeyDialog
      onRevealPublicKey={action('onRevealPublicKey')}
      onClose={action('onCancel')}
      hasReceivedWalletPublicKey={boolean('hasReceivedWalletPublicKey')}
      error={null}
      walletName={'Test Wallet'}
    />
  ),
  { decorators }
);
