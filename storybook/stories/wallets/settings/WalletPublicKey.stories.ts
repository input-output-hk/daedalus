// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean } from '@storybook/addon-knobs';

// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

// Components
import WalletPublicKeyDialog from '../../../../source/renderer/app/components/wallet/settings/WalletPublicKeyDialog';

storiesOf('Wallets|Settings', module)
  .addDecorator(WalletsWrapper)
  .addDecorator(withKnobs)

  .add('Public Key - Spending Password', () => (
    <WalletPublicKeyDialog
      onRevealPublicKey={action('onRevealPublicKey')}
      onClose={action('onCancel')}
      hasReceivedWalletPublicKey={boolean('hasReceivedWalletPublicKey')}
      error={null}
      walletName={'Test Wallet'}
    />
  ));
