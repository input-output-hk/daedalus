import React from 'react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean } from '@storybook/addon-knobs';
// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
// Components
import WalletPublicKeyDialog from '../../../../source/renderer/app/components/wallet/settings/WalletPublicKeyDialog';

export default {
  title: 'Wallets / Settings',
  decorators: [WalletsWrapper, withKnobs],
};

export const PublicKeySpendingPassword = {
  render: () => (
    <WalletPublicKeyDialog
      onRevealPublicKey={action('onRevealPublicKey')}
      onClose={action('onCancel')}
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
      hasReceivedWalletPublicKey={boolean('hasReceivedWalletPublicKey')}
      error={null}
      walletName={'Test Wallet'}
    />
  ),

  name: 'Public Key - Spending Password',
};
