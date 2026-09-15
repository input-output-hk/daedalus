import React from 'react';
// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
// Screens
import WalletSettingsScreen from './_support/WalletSettingsScreen';
import './WalletDelete.stories';
import './WalletUnpair.stories';
import './WalletRecoveryPhraseVerification.stories';
import './WalletPublicKey.stories';
import './PublicKeyQRCode.stories';
import './UndelegateWallet.stories';

export default {
  title: 'Wallets / Settings',
  decorators: [WalletsWrapper],
};

export const WalletSettings = {
  render: (_, props) => <WalletSettingsScreen {...props} />,
};
