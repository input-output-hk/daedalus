// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

// Components
import WalletPublicKeyQRCodeDialog from '../../../../source/renderer/app/components/wallet/settings/WalletPublicKeyQRCodeDialog';

storiesOf('Wallets|Settings', module)
  .addDecorator(WalletsWrapper)
  .add('Public Key - QR Code', (props) => (
    <WalletPublicKeyQRCodeDialog
      {...props}
      walletName="Wallet Public Key"
      walletPublicKey="Wallet Public Key"
      onClose={() => null}
    />
  ));
