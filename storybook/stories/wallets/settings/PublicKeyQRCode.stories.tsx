import React from 'react';
// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
// Components
import PublicKeyQRCodeDialog from '../../../../source/renderer/app/components/wallet/settings/ICOPublicKeyQRCodeDialog';

export default {
  title: 'Wallets / Settings',
  decorators: [WalletsWrapper],
};

export const PublicKeyQrCode = {
  render: (props) => (
    <PublicKeyQRCodeDialog
      {...props}
      walletName="Wallet Public Key"
      walletPublicKey="Wallet Public Key"
      onClose={() => null}
    />
  ),

  name: 'Public Key - QR Code',
};
