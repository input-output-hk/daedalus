// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

// Components
import PublicKeyQRCodeDialog from '../../../../source/renderer/app/components/wallet/settings/ICOPublicKeyQRCodeDialog';

const decorators = [WalletsWrapper];

storiesOf('Wallets/Settings', module).add(
  'Public Key - QR Code',
  (_, props) => (
    <PublicKeyQRCodeDialog
      {...props}
      walletName="Wallet Public Key"
      walletPublicKey="Wallet Public Key"
      onClose={() => null}
    />
  ),
  { decorators }
);
